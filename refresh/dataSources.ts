// Season stats from the MLB Stats API and Baseball Savant. A port of refresh/data_sources.R.
//
// One code path builds every season: buildSeasonStats(year) returns the hitter and pitcher tables for any year, current or
// finished. A finished season written to disk (buildBaselineSeason) is the same table, so the formulas for current stats and
// baselines cannot drift apart. Tables are keyed by mlbamid (the MLB player id).
//
// Where each field comes from (checked against FanGraphs for 2023-2025, see docs/fangraphs-spike-results.md):
//   hitters   counts and Age        MLB stats=season (Age is the age on June 30 of the season)
//             wOBA                  MLB stats=sabermetrics
//             xwOBA                 Savant expected_statistics (est_woba)
//             Barrels               Savant statcast (barrels, a count)
//   pitchers  counts and Age        MLB stats=season
//             FIP, BABIP, LOB%      computed from the counts (LOB% is clamped to 0-100)
//             xERA                  Savant expected_statistics (xera)
//             barrel%, O-Swing%,    Savant custom leaderboard
//             whiff%
// Hitters and pitchers are separate lists: a pitcher who batted is not a hitter, and a position player who pitched is not a
// pitcher (Savant's pitcher leaderboards have no rows for them anyway). Ohtani, whose primary position is TWP, is in both.
// Units: the percent columns (barrel_percent, o_swing_percent, whiff_percent, lob_percent) are 0-100, babip is a fraction.

import { existsSync, mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { asciiName, parseCsvText, parseNumber, roundR, writeCsv, type Cell, type Row } from "./format.ts";
import {
  add, addAll, div, hasDuplicates, leftJoin, mul, num, safeDivide, sub, sumNaRm, requireColumns, text, type Num,
} from "./helpers.ts";
import { HITTER_SEASON_COLUMNS, PITCHER_SEASON_COLUMNS } from "./types.ts";

export type Log = (message: string) => void;
export type HttpGet = (url: string) => Promise<string>;
export type Rows = Row[];

// =============================================================================
// GENERAL HELPERS
// =============================================================================

/** Prefer the MLB-side id text; an empty or "NA" id is missing. */
export function normalizeMlbamId(value: unknown): string | null {
  if (value === null || value === undefined) return null;
  const id = String(value);
  return id === "" || id === "NA" ? null : id;
}

/** Starters throw at least half their games as starts or average more than four innings an appearance (R's three-valued | ). */
export function classifyPitcherRole(gs: Num, g: Num, ip: Num): "SP" | "RP" | null {
  const starts = ratio(gs, g);
  const length = ratio(ip, g);
  const first = starts === null ? null : starts >= 0.5;
  const second = length === null ? null : length > 4;
  if (first === true || second === true) return "SP";
  if (first === null || second === null) return null;
  return "RP";
}

// x / g where 0/0 is NaN (missing) and n/0 is Infinity, as in R.
function ratio(numerator: Num, denominator: Num): Num {
  if (numerator === null || denominator === null) return null;
  const value = numerator / denominator;
  return Number.isNaN(value) ? null : value;
}

/**
 * The MLB and Savant endpoints fail transiently now and then; a few retries keep one blip from costing a day of data.
 * Waits grow between attempts (10s, then 20s).
 */
export async function withRetry<T>(
  fn: () => Promise<T>,
  label: string,
  { attempts = 3, waitSeconds = 10, log = console.log, sleep = (ms: number) => new Promise<void>((resolve) => setTimeout(resolve, ms)) }: {
    attempts?: number; waitSeconds?: number; log?: Log; sleep?: (ms: number) => Promise<void>;
  } = {},
): Promise<T> {
  let failure: unknown;
  for (let attempt = 1; attempt <= attempts; attempt++) {
    try {
      return await fn();
    } catch (error) {
      failure = error;
      log(`${label} failed (attempt ${attempt} of ${attempts}): ${messageOf(error)}`);
      if (attempt < attempts) await sleep(waitSeconds * attempt * 1000);
    }
  }
  throw new Error(`${label} failed after ${attempts} attempts: ${messageOf(failure)}`);
}

function messageOf(error: unknown): string {
  return error instanceof Error ? error.message : String(error);
}

/** MLB reports innings as "123.2" meaning 123 and two thirds. A missing value reads as 0, as in the R code. */
export function parseMlbInnings(value: unknown): number {
  if (value === null || value === undefined) return 0;
  const textValue = String(value);
  const dot = textValue.indexOf(".");
  const whole = parseNumber(dot < 0 ? textValue : textValue.slice(0, dot)) ?? 0;
  const remainder = dot < 0 ? 0 : (parseNumber(textValue.slice(dot + 1)) ?? 0);
  return whole + remainder / 3;
}

// =============================================================================
// HTTP
// =============================================================================

/** Honest and generic: says what the client is and that the volume is low, without naming anyone. */
export const DATA_SOURCE_USER_AGENT = "baseball-data-refresh (hobby project; a handful of requests per run)";

export const defaultHttpGet: HttpGet = async (url) => {
  const response = await fetch(url, {
    headers: { "User-Agent": DATA_SOURCE_USER_AGENT },
    signal: AbortSignal.timeout(60_000),
  });
  if (!response.ok) throw new Error(`status ${response.status}`);
  return await response.text();
};

export type FetchOptions = { httpGet?: HttpGet; attempts?: number; waitSeconds?: number; log?: Log };

export function fetchText(url: string, label: string, options: FetchOptions = {}): Promise<string> {
  const { httpGet = defaultHttpGet, attempts = 3, waitSeconds, log } = options;
  return withRetry(() => httpGet(url), label, { attempts, waitSeconds, log });
}

// =============================================================================
// MLB STATS API
// =============================================================================

export function mlbStatsUrl(stats: string, group: string, year: number, extra = ""): string {
  return `https://statsapi.mlb.com/api/v1/stats?stats=${stats}&group=${group}&playerPool=ALL&season=${year}${extra}&limit=10000`;
}

type JsonObject = { [key: string]: unknown };

// jsonlite's fromJSON(flatten = TRUE): nested objects become dotted column names; a field a row lacks is missing.
function flatten(value: unknown, prefix: string, into: JsonObject): void {
  if (value !== null && typeof value === "object" && !Array.isArray(value)) {
    for (const [key, inner] of Object.entries(value as JsonObject)) flatten(inner, prefix ? `${prefix}.${key}` : key, into);
  } else {
    into[prefix] = value;
  }
}

/**
 * The splits of one stats response as a flat table. The feeds used here return one row per player (a traded player's teams are
 * already combined); a duplicate id would be counted twice, so it stops the run.
 */
export function parseMlbStats(body: string, label: string, minRows = 1, oneRowPerPlayer = true): { columns: string[]; rows: Row[] } {
  const parsed = JSON.parse(body) as { stats?: { splits?: unknown[] }[] };
  const splits = parsed?.stats?.[0]?.splits;
  if (!Array.isArray(splits) || splits.length === 0) throw new Error(`${label} returned no rows`);

  const flat = splits.map((split) => {
    const into: JsonObject = {};
    flatten(split, "", into);
    return into;
  });
  const columns: string[] = [];
  for (const row of flat) for (const key of Object.keys(row)) if (!columns.includes(key)) columns.push(key);
  requireColumns(columns, ["player.id"], label);

  let rows: Row[] = flat.map((row) => {
    const out: Row = {};
    for (const column of columns) out[column] = toCell(row[column]);
    return out;
  });
  // Some feeds mix in minor-league rows (sport.id other than 1); only the majors count.
  if (columns.includes("sport.id")) rows = rows.filter((row) => parseNumber(row["sport.id"]) === 1);
  if (oneRowPerPlayer && hasDuplicates(rows.map((row) => row["player.id"]))) {
    throw new Error(`${label} has more than one row for a player; the one-row-per-player assumption no longer holds`);
  }
  if (rows.length < minRows) throw new Error(`${label} returned ${rows.length} rows, fewer than the ${minRows} expected`);
  return { columns, rows };
}

function toCell(value: unknown): Cell {
  if (value === null || value === undefined) return null;
  if (typeof value === "number" || typeof value === "string") return value;
  if (typeof value === "boolean") return value ? 1 : 0;
  return null;
}

type FetchSeasonOptions = FetchOptions & { minRows?: number; oneRowPerPlayer?: boolean };

async function fetchMlbStats(year: number, group: string, stats: string, options: FetchSeasonOptions) {
  const label = `MLB Stats API ${stats} ${group} ${year}`;
  const extra = stats === "season" ? "&hydrate=person(currentTeam,primaryPosition)" : "";
  const body = await fetchText(mlbStatsUrl(stats, group, year, extra), label, options);
  return { label, table: parseMlbStats(body, label, options.minRows ?? 1, options.oneRowPerPlayer ?? true) };
}

const number = (row: Row, column: string): Num => parseNumber(row[column]);

/** Season hitting counts for every player with a plate appearance or not; buildHitterSeason decides who counts. */
export async function fetchMlbHittingSeason(year: number, options: FetchSeasonOptions = {}): Promise<Rows> {
  const { label, table } = await fetchMlbStats(year, "hitting", "season", options);
  requireColumns(table.columns, [
    "player.fullName", "player.primaryPosition.abbreviation", "stat.age", "stat.gamesPlayed", "stat.atBats", "stat.plateAppearances",
    "stat.hits", "stat.doubles", "stat.triples", "stat.homeRuns", "stat.baseOnBalls", "stat.hitByPitch", "stat.sacFlies",
    "stat.strikeOuts", "stat.totalBases",
  ], label);
  return table.rows.map((row) => ({
    mlbamid: normalizeMlbamId(row["player.id"]),
    Name: asciiName(text(row, "player.fullName") ?? (row["player.fullName"] === null ? null : String(row["player.fullName"]))),
    Age: number(row, "stat.age"),
    primary_position: text(row, "player.primaryPosition.abbreviation"),
    G: number(row, "stat.gamesPlayed"),
    AB: number(row, "stat.atBats"),
    PA: number(row, "stat.plateAppearances"),
    H: number(row, "stat.hits"),
    "2B": number(row, "stat.doubles"),
    "3B": number(row, "stat.triples"),
    HR: number(row, "stat.homeRuns"),
    BB: number(row, "stat.baseOnBalls"),
    HBP: number(row, "stat.hitByPitch"),
    SF: number(row, "stat.sacFlies"),
    SO: number(row, "stat.strikeOuts"),
    TB: number(row, "stat.totalBases"),
  }));
}

export async function fetchMlbPitchingSeason(year: number, options: FetchSeasonOptions = {}): Promise<Rows> {
  const { label, table } = await fetchMlbStats(year, "pitching", "season", options);
  requireColumns(table.columns, [
    "player.fullName", "player.primaryPosition.abbreviation", "stat.age", "stat.gamesPitched", "stat.gamesStarted",
    "stat.inningsPitched", "stat.battersFaced", "stat.hits", "stat.runs", "stat.earnedRuns", "stat.homeRuns", "stat.strikeOuts",
    "stat.baseOnBalls", "stat.intentionalWalks", "stat.hitByPitch", "stat.atBats", "stat.sacFlies",
  ], label);
  return table.rows.map((row) => ({
    mlbamid: normalizeMlbamId(row["player.id"]),
    name: asciiName(text(row, "player.fullName") ?? (row["player.fullName"] === null ? null : String(row["player.fullName"]))),
    age: number(row, "stat.age"),
    primary_position: text(row, "player.primaryPosition.abbreviation"),
    g: number(row, "stat.gamesPitched"),
    gs: number(row, "stat.gamesStarted"),
    ip: parseMlbInnings(row["stat.inningsPitched"]),
    tbf: number(row, "stat.battersFaced"),
    h: number(row, "stat.hits"),
    r: number(row, "stat.runs"),
    er: number(row, "stat.earnedRuns"),
    hr: number(row, "stat.homeRuns"),
    so: number(row, "stat.strikeOuts"),
    bb: number(row, "stat.baseOnBalls"),
    ibb: number(row, "stat.intentionalWalks"),
    hbp: number(row, "stat.hitByPitch"),
    ab: number(row, "stat.atBats"),
    sf: number(row, "stat.sacFlies"),
  }));
}

/** wOBA for every hitter the MLB sabermetrics feed knows (it matched FanGraphs' wOBA to within 0.00002). */
export async function fetchMlbHitterWoba(year: number, options: FetchSeasonOptions = {}): Promise<Rows> {
  const { label, table } = await fetchMlbStats(year, "hitting", "sabermetrics", options);
  requireColumns(table.columns, ["stat.woba"], label);
  return table.rows.map((row) => ({ mlbamid: normalizeMlbamId(row["player.id"]), wOBA: number(row, "stat.woba") }));
}

// =============================================================================
// BASEBALL SAVANT
// =============================================================================
// Savant has no documented API; these are the CSV links behind the public leaderboards. The files start with a byte-order mark,
// quote some numbers, and name the first column "last_name, first_name". player_id is the MLB id.

export function savantUrl(leaderboard: string, year: number, type: string): string {
  const base = "https://baseballsavant.mlb.com/leaderboard/";
  if (leaderboard === "custom") {
    return (
      `${base}custom?year=${year}&type=${type}&filter=&min=1` +
      "&selections=pa,babip,barrel_batted_rate,oz_swing_percent,whiff_percent" +
      "&chart=false&x=pa&y=pa&r=no&chartType=beeswarm&csv=true"
    );
  }
  return `${base}${leaderboard}?type=${type}&year=${year}&position=&team=&min=1&csv=true`;
}

export function parseSavantCsv(body: string | null, requiredColumns: readonly string[], label: string, minRows = 1): { columns: string[]; rows: Row[] } {
  if (body === null || body.trim() === "") throw new Error(`${label} returned an empty response`);
  // A bot challenge or error page comes back as HTML with a 200 or 403; say so instead of failing on columns.
  if (/^\s*</i.test(body)) throw new Error(`${label} returned HTML instead of CSV (a bot challenge or an error page)`);
  const { header, records } = parseCsvText(body);
  requireColumns(header, ["player_id", ...requiredColumns], label);
  const rows: Row[] = records.map((record) => {
    const row: Row = {};
    header.forEach((name, index) => {
      row[name] = record[index] ?? null;
    });
    row["mlbamid"] = normalizeMlbamId(row["player_id"]);
    return row;
  });
  const ids = rows.map((row) => row["mlbamid"]);
  if (ids.some((id) => id === null) || hasDuplicates(ids)) throw new Error(`${label} has a missing or repeated player_id`);
  if (rows.length < minRows) throw new Error(`${label} returned ${rows.length} rows, fewer than the ${minRows} expected`);
  return { columns: [...header, "mlbamid"], rows };
}

async function fetchSavantCsv(leaderboard: string, year: number, type: string, requiredColumns: readonly string[], options: FetchSeasonOptions) {
  const label = `Savant ${leaderboard} ${type} ${year}`;
  const body = await fetchText(savantUrl(leaderboard, year, type), label, options);
  return parseSavantCsv(body, requiredColumns, label, options.minRows ?? 1);
}

export async function fetchSavantHitterXwoba(year: number, options: FetchSeasonOptions = {}): Promise<Rows> {
  const table = await fetchSavantCsv("expected_statistics", year, "batter", ["est_woba"], options);
  return table.rows.map((row) => ({ mlbamid: row["mlbamid"]!, xwOBA: number(row, "est_woba") }));
}

export async function fetchSavantHitterBarrels(year: number, options: FetchSeasonOptions = {}): Promise<Rows> {
  const table = await fetchSavantCsv("statcast", year, "batter", ["barrels"], options);
  return table.rows.map((row) => ({ mlbamid: row["mlbamid"]!, Barrels: number(row, "barrels") }));
}

export async function fetchSavantPitcherXera(year: number, options: FetchSeasonOptions = {}): Promise<Rows> {
  const table = await fetchSavantCsv("expected_statistics", year, "pitcher", ["xera"], options);
  return table.rows.map((row) => ({ mlbamid: row["mlbamid"]!, x_era: number(row, "xera") }));
}

export async function fetchSavantPitcherCustom(year: number, options: FetchSeasonOptions = {}): Promise<Rows> {
  const table = await fetchSavantCsv("custom", year, "pitcher", ["barrel_batted_rate", "oz_swing_percent", "whiff_percent"], options);
  return table.rows.map((row) => ({
    mlbamid: row["mlbamid"]!,
    barrel_percent: number(row, "barrel_batted_rate"),
    o_swing_percent: number(row, "oz_swing_percent"),
    whiff_percent: number(row, "whiff_percent"),
  }));
}

// =============================================================================
// COMPUTED FIELDS
// =============================================================================

/**
 * FanGraphs' FIP in these files counts intentional walks (BB + HBP); leaving them out did not reproduce it. The constant makes
 * league FIP equal league ERA, computed per season from the league totals.
 */
export function fipConstant(rows: Rows): Num {
  const total = (column: string) => sumNaRm(rows.map((row) => num(row, column)));
  const ip = total("ip");
  const leagueEra = div(9 * total("er"), ip);
  return sub(leagueEra, div(13 * total("hr") + 3 * (total("bb") + total("hbp")) - 2 * total("so"), ip));
}

export function computeFip(hr: Num, bb: Num, hbp: Num, so: Num, ip: Num, constant: Num): Num {
  const numerator = sub(add(mul(13, hr), mul(3, add(bb, hbp))), mul(2, so));
  return add(safeDivide(numerator, ip), constant);
}

export function computeBabip(h: Num, hr: Num, ab: Num, so: Num, sf: Num): Num {
  return safeDivide(sub(h, hr), add(sub(sub(ab, so), hr), sf));
}

/**
 * Left on base %, as a percent. Clamped to 0-100 because a pitcher with a single inning can land outside it; NA when nobody
 * could have scored (no baserunners, or one home run and nothing else).
 */
export function computeLobPercent(h: Num, bb: Num, hbp: Num, hr: Num, r: Num): Num {
  const baserunners = addAll(h, bb, hbp);
  const denominator = sub(baserunners, mul(1.4, hr));
  if (denominator === null || denominator <= 0) return null;
  const lob = div(mul(100, sub(baserunners, r)), denominator);
  return lob === null ? null : Math.min(100, Math.max(0, lob));
}

// =============================================================================
// SEASON TABLES
// =============================================================================

const byMlbamid = (rows: Rows): Map<string, Row> => new Map(rows.map((row) => [row["mlbamid"] as string, row]));
const numericId = (row: Row): number => Number(row["mlbamid"]);

/**
 * Everyone with a plate appearance, except pitchers who happened to bat. Ohtani's primary position is TWP, so Ohtani stays. A
 * hitter Savant has no batted-ball row for (a few players with one to three PA) has no barrels, so 0.
 */
export function buildHitterSeason(mlbHitting: Rows, mlbWoba: Rows, savantXwoba: Rows, savantBarrels: Rows): Rows {
  const woba = byMlbamid(mlbWoba);
  const xwoba = byMlbamid(savantXwoba);
  const barrels = byMlbamid(savantBarrels);
  const kept = mlbHitting.filter((row) => {
    const pa = num(row, "PA");
    const position = text(row, "primary_position");
    return pa !== null && pa > 0 && (position === null || position !== "P");
  });
  const rows = kept.map((row) => {
    const barrelCount = num(barrels.get(row["mlbamid"] as string) ?? {}, "Barrels");
    const out: Row = {};
    for (const column of HITTER_SEASON_COLUMNS) {
      if (column === "Barrels") out[column] = barrelCount === null ? 0 : barrelCount;
      else if (column === "wOBA") out[column] = num(woba.get(row["mlbamid"] as string) ?? {}, "wOBA");
      else if (column === "xwOBA") out[column] = num(xwoba.get(row["mlbamid"] as string) ?? {}, "xwOBA");
      else out[column] = row[column] ?? null;
    }
    return out;
  });
  return rows.sort((a, b) => numericId(a) - numericId(b));
}

/**
 * Everyone who faced a batter, except position players who pitched in a blowout (Ohtani's primary position is TWP, so Ohtani
 * stays). The FIP constant uses the whole league, position players included, so it is computed before anyone is filtered out.
 */
export function buildPitcherSeason(mlbPitching: Rows, savantXera: Rows, savantCustom: Rows): Rows {
  const constant = fipConstant(mlbPitching);
  const xera = byMlbamid(savantXera);
  const custom = byMlbamid(savantCustom);
  const kept = mlbPitching.filter((row) => {
    const tbf = num(row, "tbf");
    const position = text(row, "primary_position");
    return tbf !== null && tbf > 0 && (position === null || position === "P" || position === "TWP");
  });
  const rows = kept.map((row) => {
    const id = row["mlbamid"] as string;
    const x = xera.get(id) ?? {};
    const c = custom.get(id) ?? {};
    return {
      mlbamid: id,
      name: row["name"] ?? null,
      age: row["age"] ?? null,
      position: classifyPitcherRole(num(row, "gs"), num(row, "g"), num(row, "ip")),
      g: row["g"] ?? null,
      gs: row["gs"] ?? null,
      ip: roundR(num(row, "ip"), 6),
      tbf: row["tbf"] ?? null,
      h: row["h"] ?? null,
      r: row["r"] ?? null,
      er: row["er"] ?? null,
      hr: row["hr"] ?? null,
      so: row["so"] ?? null,
      bb: row["bb"] ?? null,
      ibb: row["ibb"] ?? null,
      hbp: row["hbp"] ?? null,
      ab: row["ab"] ?? null,
      sf: row["sf"] ?? null,
      x_era: num(x, "x_era"),
      fip: roundR(computeFip(num(row, "hr"), num(row, "bb"), num(row, "hbp"), num(row, "so"), num(row, "ip"), constant), 6),
      barrel_percent: num(c, "barrel_percent"),
      o_swing_percent: num(c, "o_swing_percent"),
      whiff_percent: num(c, "whiff_percent"),
      babip: roundR(computeBabip(num(row, "h"), num(row, "hr"), num(row, "ab"), num(row, "so"), num(row, "sf")), 6),
      lob_percent: roundR(computeLobPercent(num(row, "h"), num(row, "bb"), num(row, "hbp"), num(row, "hr"), num(row, "r")), 6),
    } satisfies Record<(typeof PITCHER_SEASON_COLUMNS)[number], Cell>;
  });
  return rows.sort((a, b) => numericId(a) - numericId(b));
}

export type MinRows = {
  mlbHitting: number; mlbPitching: number; mlbWoba: number; savantXwoba: number; savantBarrels: number; savantXera: number; savantCustom: number;
};

/** Fewest rows each feed should return for a full season (2023-2025 returned 650-870). A thin feed should stop the run. */
export const FULL_SEASON_MIN_ROWS: MinRows = {
  mlbHitting: 600, mlbPitching: 700, mlbWoba: 600, savantXwoba: 600, savantBarrels: 600, savantXera: 700, savantCustom: 700,
};
/** For a season in progress: only rule out an empty or truncated response. */
export const ANY_SEASON_MIN_ROWS: MinRows = {
  mlbHitting: 1, mlbPitching: 1, mlbWoba: 1, savantXwoba: 1, savantBarrels: 1, savantXera: 1, savantCustom: 1,
};

export type SeasonStats = { hitters: Rows; pitchers: Rows };

/** Seven requests. */
export async function buildSeasonStats(
  year: number,
  { minRows = FULL_SEASON_MIN_ROWS, log = console.log, ...options }: FetchOptions & { minRows?: MinRows } = {},
): Promise<SeasonStats> {
  log(`Building ${year} season stats from the MLB Stats API and Baseball Savant`);
  const withMin = (minimum: number): FetchSeasonOptions => ({ ...options, log, minRows: minimum });
  const mlbHitting = await fetchMlbHittingSeason(year, withMin(minRows.mlbHitting));
  const mlbWoba = await fetchMlbHitterWoba(year, withMin(minRows.mlbWoba));
  const savantXwoba = await fetchSavantHitterXwoba(year, withMin(minRows.savantXwoba));
  const savantBarrels = await fetchSavantHitterBarrels(year, withMin(minRows.savantBarrels));
  const hitters = buildHitterSeason(mlbHitting, mlbWoba, savantXwoba, savantBarrels);

  const mlbPitching = await fetchMlbPitchingSeason(year, withMin(minRows.mlbPitching));
  const savantXera = await fetchSavantPitcherXera(year, withMin(minRows.savantXera));
  const savantCustom = await fetchSavantPitcherCustom(year, withMin(minRows.savantCustom));
  return { hitters, pitchers: buildPitcherSeason(mlbPitching, savantXera, savantCustom) };
}

// =============================================================================
// BASELINE FILES
// =============================================================================

export function baselineFile(kind: "hitters" | "pitchers", year: number, dir: string): string {
  return join(dir, `baseline-${kind}-${year}.csv`);
}

/**
 * Writes baseline-hitters-<year>.csv and baseline-pitchers-<year>.csv for a finished season. Refuses to replace existing files
 * unless force is true, and writes neither file if either already exists.
 */
export function writeBaselineSeason(year: number, stats: SeasonStats, dir: string, force = false, log: Log = console.log): { hitters: string; pitchers: string } {
  const files = { hitters: baselineFile("hitters", year, dir), pitchers: baselineFile("pitchers", year, dir) };
  const existing = Object.values(files).filter((file) => existsSync(file));
  if (existing.length > 0 && !force) throw new Error(`Refusing to overwrite ${existing.join(", ")}; pass --force to replace it`);
  mkdirSync(dir, { recursive: true });
  writeFileSync(files.hitters, writeCsv(HITTER_SEASON_COLUMNS, stats.hitters));
  writeFileSync(files.pitchers, writeCsv(PITCHER_SEASON_COLUMNS, stats.pitchers));
  log(`Wrote ${files.hitters} ( ${stats.hitters.length} hitters ) and ${files.pitchers} ( ${stats.pitchers.length} pitchers )`);
  return files;
}

export async function buildBaselineSeason(
  year: number,
  { force = false, dir, ...options }: FetchOptions & { force?: boolean; dir: string },
): Promise<{ hitters: string; pitchers: string }> {
  const files = [baselineFile("hitters", year, dir), baselineFile("pitchers", year, dir)];
  const existing = files.filter((file) => existsSync(file));
  if (existing.length > 0 && !force) throw new Error(`Refusing to overwrite ${existing.join(", ")}; pass --force to replace it`);
  return writeBaselineSeason(year, await buildSeasonStats(year, options), dir, force, options.log);
}
