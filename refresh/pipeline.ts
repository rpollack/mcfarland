// Transformations, baselines, frozen player ids and the gates that guard the published CSVs. A port of refresh/pipeline.R.

import { existsSync, readFileSync } from "node:fs";
import { compareCodepoints, readCsvTable, roundR, type Cell, type Json, type Row, rVector } from "./format.ts";
import { baselineFile, type Rows } from "./dataSources.ts";
import {
  add, addAll, groupBy, hasDuplicates, intersect, leftJoin, mul, num, requireColumns, safeDivide, setdiff, sub, sumKeepNa, sumNaRm,
  weightedMean, type Num,
} from "./helpers.ts";
import {
  HITTER_BASELINE_COLUMNS, HITTER_METRIC_COLS, HITTER_OUTPUT_COLUMNS, PITCHER_BASELINE_COLUMNS, PITCHER_METRIC_COLS, PITCHER_OUTPUT_COLUMNS,
  PITCHER_SEASON_COLUMNS, PLAYER_LOOKUP_COLUMNS, SERVED_INVENTORY_COLUMNS, TEXT_OUTPUT_COLUMNS, type Table,
} from "./types.ts";

// =============================================================================
// SHARED AGGREGATION
// =============================================================================
// The same hitter and pitcher metrics are needed three ways: per player over the baseline seasons, league-wide over the baseline
// seasons, and (hitters) per player and league-wide for the current season. Each way feeds the same summary function so the
// formulas cannot drift apart. Input rows carry a weight column `w`: the season weight for baselines, 1 for the current season.

const HITTER_SUMMARY_COLUMNS = [...HITTER_METRIC_COLS, "PA"];
const PITCHER_BASELINE_SUMMARY_COLUMNS = [
  "era", "k_percent", "bb_percent", "k_minus_bb_percent", "xera", "era_xera_gap", "fip", "era_fip_gap", "barrel_percent", "o_swing_percent",
  "babip", "lob_percent", "whiff_percent",
];

/** sum(column * w) with R's na.rm switch: a missing product is skipped, or poisons the sum. */
function weightedSum(rows: Rows, column: string, naRm: boolean): Num {
  const products = rows.map((row) => mul(num(row, column), num(row, "w")));
  return naRm ? sumNaRm(products) : sumKeepNa(products);
}

export function summarizeHitterMetrics(rows: Rows, naRm = true): Row {
  const sum = (column: string) => weightedSum(rows, column, naRm);
  const pa = sum("PA");
  const weightedPa = rows.map((row) => mul(num(row, "PA"), num(row, "w")));
  const wOBA = weightedMean(rows.map((row) => num(row, "wOBA")), weightedPa);
  const xwOBA = weightedMean(rows.map((row) => num(row, "xwOBA")), weightedPa);
  return {
    AVG: safeDivide(sum("H"), sum("AB")),
    OBP: safeDivide(addAll(sum("H"), sum("BB"), sum("HBP")), addAll(sum("AB"), sum("BB"), sum("HBP"), sum("SF"))),
    SLG: safeDivide(sum("TB"), sum("AB")),
    K_pct: mul(100, safeDivide(sum("SO"), pa)),
    BB_pct: mul(100, safeDivide(sum("BB"), pa)),
    Barrel_pct: mul(100, safeDivide(sum("Barrels"), pa)),
    BABIP: safeDivide(sub(sum("H"), sum("HR")), add(sub(sub(sum("AB"), sum("SO")), sum("HR")), sum("SF"))),
    wOBA,
    xwOBA,
    xwOBA_wOBA_gap: sub(xwOBA, wOBA),
    // PA is the plain total across seasons: the weights only apply to the rate numerators and denominators above.
    PA: naRm ? sumNaRm(rows.map((row) => num(row, "PA"))) : sumKeepNa(rows.map((row) => num(row, "PA"))),
  };
}

/** The baseline files already hold percents (0-100), so nothing is rescaled here. */
export function summarizePitcherBaselineMetrics(rows: Rows, naRm = true): Row {
  const sum = (column: string) => weightedSum(rows, column, naRm);
  const weightedTbf = rows.map((row) => mul(num(row, "tbf"), num(row, "w")));
  const mean = (column: string) => weightedMean(rows.map((row) => num(row, column)), weightedTbf);
  const era = mul(safeDivide(sum("er"), sum("ip")), 9);
  const kPercent = mul(safeDivide(sum("so"), sum("tbf")), 100);
  const bbPercent = mul(safeDivide(sum("bb"), sum("tbf")), 100);
  const xera = mean("x_era");
  const fip = mean("fip");
  return {
    era,
    k_percent: kPercent,
    bb_percent: bbPercent,
    k_minus_bb_percent: sub(kPercent, bbPercent),
    xera,
    era_xera_gap: sub(era, xera),
    fip,
    era_fip_gap: sub(era, fip),
    barrel_percent: mean("barrel_percent"),
    o_swing_percent: mean("o_swing_percent"),
    babip: mean("babip"),
    lob_percent: mean("lob_percent"),
    whiff_percent: mean("whiff_percent"),
  };
}

// =============================================================================
// BASELINES (weighted history)
// =============================================================================
// The baseline for a player is the weighted history of the three seasons before the current one (weights 1, 3, 5, oldest first).
// Each season is a baseline-hitters-<year>.csv / baseline-pitchers-<year>.csv in data/baselines/, written by
// `npm run refresh:baseline -- <year>`, keyed by mlbamid.

export function readBaselineSeasons(
  kind: "hitters" | "pitchers",
  seasons: { year: number; weight: number }[],
  requiredColumns: readonly string[],
  dir: string,
): Rows {
  const all: Rows = [];
  for (const { year, weight } of seasons) {
    const file = baselineFile(kind, year, dir);
    if (!existsSync(file)) {
      throw new Error(`Baseline season file ${file} is missing; run npm run refresh:baseline -- ${year} first.`);
    }
    const table = readCsvTable(readFileSync(file, "utf8"), ["mlbamid"]);
    requireColumns(table.columns, requiredColumns, file);
    for (const row of table.rows) all.push({ ...row, year, w: weight });
  }
  return all;
}

export const readHitterBaselineSeasons = (seasons: { year: number; weight: number }[], dir: string) =>
  readBaselineSeasons("hitters", seasons, HITTER_BASELINE_COLUMNS, dir);
export const readPitcherBaselineSeasons = (seasons: { year: number; weight: number }[], dir: string) =>
  readBaselineSeasons("pitchers", seasons, PITCHER_BASELINE_COLUMNS, dir);

// =============================================================================
// CURRENT SEASON
// =============================================================================

/** One row per hitter (the season table already has one), with the same metrics as the baseline summaries. */
export function combineHitterCurrentStats(hitters: Rows): Table {
  const rows = groupBy(hitters.map((row) => ({ ...row, w: 1 })), ["mlbamid", "Name"]).map(({ key, rows: group }) => ({
    mlbamid: key[0]!,
    Name: key[1]!,
    ...summarizeHitterMetrics(group),
    Age: group[0]!["Age"] ?? null,
  }));
  return { columns: ["mlbamid", "Name", ...HITTER_SUMMARY_COLUMNS, "Age"], rows };
}

export function combinePitcherCurrentStats(pitchers: Rows, currentYear: number): Table {
  const rows = pitchers.map((row) => {
    const kPercent = mul(100, safeDivide(num(row, "so"), num(row, "tbf")));
    const bbPercent = mul(100, safeDivide(num(row, "bb"), num(row, "tbf")));
    const era = mul(9, safeDivide(num(row, "er"), num(row, "ip")));
    const out: Row = {};
    for (const column of PITCHER_SEASON_COLUMNS) {
      if (column !== "r" && column !== "ab" && column !== "sf") out[column] = row[column] ?? null;
    }
    out["season"] = currentYear;
    out["xera"] = num(row, "x_era");
    out["k_percent"] = kPercent;
    out["bb_percent"] = bbPercent;
    out["k_minus_bb_percent"] = sub(kPercent, bbPercent);
    out["era"] = era;
    out["xera_gap"] = sub(era, num(row, "x_era"));
    out["fip_era_gap"] = sub(era, num(row, "fip"));
    return out;
  });
  const columns = [
    ...PITCHER_SEASON_COLUMNS.filter((column) => column !== "r" && column !== "ab" && column !== "sf"),
    "season", "xera", "k_percent", "bb_percent", "k_minus_bb_percent", "era", "xera_gap", "fip_era_gap",
  ];
  return { columns, rows };
}

export function buildPitcherLeagueCurrent(rows: Rows): Row {
  const total = (column: string) => sumNaRm(rows.map((row) => num(row, column)));
  const weights = rows.map((row) => num(row, "tbf"));
  const mean = (column: string) => weightedMean(rows.map((row) => num(row, column)), weights);
  const kPercent = mul(100, safeDivide(total("so"), total("tbf")));
  const bbPercent = mul(100, safeDivide(total("bb"), total("tbf")));
  return {
    era: mul(9, safeDivide(total("er"), total("ip"))),
    k_percent: kPercent,
    bb_percent: bbPercent,
    k_minus_bb_percent: sub(kPercent, bbPercent),
    xera: mean("xera"),
    o_swing_percent: mean("o_swing_percent"),
    whiff_percent: mean("whiff_percent"),
    barrel_percent: mean("barrel_percent"),
    lob_percent: mean("lob_percent"),
    babip: mean("babip"),
  };
}

// =============================================================================
// DIFFS AGAINST BASELINE
// =============================================================================
// `rows` hold <metric>_cur and <metric>_l3 columns from joining current stats to the baseline.

export function addBaselineDiffs(rows: Rows, metricCols: readonly string[]): void {
  for (const row of rows) for (const metric of metricCols) row[`${metric}_diff`] = sub(num(row, `${metric}_cur`), num(row, `${metric}_l3`));
}

export function buildLeagueDiffs(current: Row, baseline: Row, metricCols: readonly string[]): Record<string, Num> {
  const diffs: Record<string, Num> = {};
  for (const metric of metricCols) diffs[metric] = sub(num(current, metric), num(baseline, metric));
  return diffs;
}

export function addLeagueAdjustedDiffs(rows: Rows, metricCols: readonly string[], leagueDiffs: Record<string, Num>): void {
  for (const row of rows) {
    for (const metric of metricCols) row[`${metric}_lg_adj_diff`] = sub(num(row, `${metric}_diff`), leagueDiffs[metric] ?? null);
  }
}

/** mutate(across(where(is.numeric), ~ round(.x, 3))) */
function roundNumericCells(rows: Rows, digits: number): void {
  for (const row of rows) {
    for (const column of Object.keys(row)) {
      const value = row[column];
      if (typeof value === "number") row[column] = roundR(value, digits);
    }
  }
}

// =============================================================================
// FROZEN PLAYER IDS
// =============================================================================
// PlayerId is the app's key for a player (URLs, votes, trend rows, cached analyses, stars). The first ids were FanGraphs ids, which
// this pipeline no longer has, so they are frozen in data/player_id_map.csv (mlbamid, PlayerId), built once from the committed
// player_lookup.csv and never regenerated: losing or rebuilding it would change every existing player's id. A player in the map
// keeps the PlayerId; anyone else gets PlayerId = mlbamid. So the column holds two eras of ids forever: 5-digit legacy ones and
// 6-digit MLB ones. The two ranges cannot overlap (legacy ids top out at 37,238; MLB ids start at 434,378), and
// checkPlayerIdCollisions enforces that.

export function readPlayerIdMap(file: string): Map<string, string> {
  const textColumns = ["mlbamid", "PlayerId"];
  const table = readCsvTable(readFileSync(file, "utf8"), textColumns);
  requireColumns(table.columns, ["mlbamid", "PlayerId"], file);
  const mlbamids = table.rows.map((row) => row["mlbamid"]);
  const playerIds = table.rows.map((row) => row["PlayerId"]);
  if (hasDuplicates(mlbamids) || hasDuplicates(playerIds)) throw new Error(`${file} maps a player or an id more than once`);
  return new Map(table.rows.map((row) => [row["mlbamid"] as string, row["PlayerId"] as string]));
}

/** A player in the map keeps the frozen PlayerId; anyone else gets the mlbamid. */
export function addPlayerIds(rows: Rows, idMap: ReadonlyMap<string, string>): void {
  for (const row of rows) {
    const mlbamid = row["mlbamid"] === null || row["mlbamid"] === undefined ? null : String(row["mlbamid"]);
    row["mlbamid"] = mlbamid;
    row["PlayerId"] = (mlbamid === null ? undefined : idMap.get(mlbamid)) ?? mlbamid;
  }
}

/** A new player's PlayerId is the mlbamid; that must never equal a mapped legacy id, or two players would share a key. */
export function checkPlayerIdCollisions(rows: Rows, idMap: ReadonlyMap<string, string>): void {
  const unmapped = rows.map((row) => row["mlbamid"]).filter((id): id is string => typeof id === "string" && !idMap.has(id));
  const clashes = intersect(unmapped, [...idMap.values()]);
  if (clashes.length > 0) throw new Error(`New players have an mlbamid equal to a frozen legacy PlayerId: ${clashes.join(", ")}`);
}

// =============================================================================
// PUBLISHED TABLES
// =============================================================================

export type PublishedTables = { hitters: Table; pitchers: Table };

export function buildHitterOutput(
  currentHitters: Rows,
  baselineHistory: Rows,
  idMap: ReadonlyMap<string, string>,
): Table {
  const last3 = {
    columns: ["mlbamid", ...HITTER_SUMMARY_COLUMNS],
    rows: groupBy(baselineHistory, ["mlbamid"]).map(({ key, rows }) => ({ mlbamid: key[0]!, ...summarizeHitterMetrics(rows, false) })),
  };
  const leagueBaseline = summarizeHitterMetrics(baselineHistory);
  const current = combineHitterCurrentStats(currentHitters);
  const leagueCurrent = summarizeHitterMetrics(currentHitters.map((row) => ({ ...row, w: 1 })));
  const leagueDiffs = buildLeagueDiffs(leagueCurrent, leagueBaseline, HITTER_METRIC_COLS);

  const joined = leftJoin(current, last3, ["mlbamid"], ["_cur", "_l3"]);
  addBaselineDiffs(joined.rows, HITTER_METRIC_COLS);
  for (const row of joined.rows) row["player_type"] = "hitter";
  addLeagueAdjustedDiffs(joined.rows, HITTER_METRIC_COLS, leagueDiffs);
  roundNumericCells(joined.rows, 3);
  addPlayerIds(joined.rows, idMap);
  return selectColumns(joined.rows, HITTER_OUTPUT_COLUMNS);
}

export function buildPitcherOutput(
  currentPitchers: Rows,
  baselineHistory: Rows,
  currentYear: number,
  idMap: ReadonlyMap<string, string>,
): Table {
  const last3 = {
    columns: ["mlbamid", "position", ...PITCHER_BASELINE_SUMMARY_COLUMNS],
    rows: groupBy(baselineHistory, ["mlbamid", "position"]).map(({ key, rows }) => ({
      mlbamid: key[0]!, position: key[1]!, ...summarizePitcherBaselineMetrics(rows, false),
    })),
  };
  const leagueBaseline = summarizePitcherBaselineMetrics(baselineHistory);
  const current = combinePitcherCurrentStats(currentPitchers, currentYear);
  const leagueCurrent = buildPitcherLeagueCurrent(current.rows);
  const leagueDiffs = buildLeagueDiffs(leagueCurrent, leagueBaseline, PITCHER_METRIC_COLS);

  // Baselines are role-specific (SP vs RP), so match on mlbamid and position. If a player ever has two baseline rows for one role,
  // keep the first so the join cannot duplicate current-season rows.
  const seen = new Set<string>();
  const byRole = {
    columns: last3.columns,
    rows: last3.rows.filter((row) => {
      const key = JSON.stringify([row["mlbamid"], row["position"]]);
      if (seen.has(key)) return false;
      seen.add(key);
      return true;
    }),
  };

  const joined = leftJoin(current, byRole, ["mlbamid", "position"], ["_cur", "_l3"]);
  addBaselineDiffs(joined.rows, PITCHER_METRIC_COLS);
  for (const row of joined.rows) row["player_type"] = "pitcher";
  addLeagueAdjustedDiffs(joined.rows, PITCHER_METRIC_COLS, leagueDiffs);
  roundNumericCells(joined.rows, 3);
  for (const row of joined.rows) {
    row["Name"] = row["name"] ?? null;
    row["Age"] = row["age"] ?? null;
  }
  addPlayerIds(joined.rows, idMap);
  return selectColumns(joined.rows, PITCHER_OUTPUT_COLUMNS);
}

function selectColumns(rows: Rows, columns: readonly string[]): Table {
  for (const column of columns) {
    if (rows.length > 0 && !(column in rows[0]!)) throw new Error(`Expected output column ${column} was not built`);
  }
  return { columns: [...columns], rows: rows.map((row) => Object.fromEntries(columns.map((column) => [column, row[column] ?? null]))) };
}

// =============================================================================
// LOOKUP AND VALIDATION
// =============================================================================

export function buildPlayerLookup(hitters: Table, pitchers: Table): Table {
  const hitterNames = hitters.rows.map((row) => row["Name"]);
  const duplicateNames = new Set(intersect(hitterNames, pitchers.rows.map((row) => row["Name"])));
  const rows: Rows = [
    ...hitters.rows.map((row) => ({
      Name: row["Name"]!, PlayerId: row["PlayerId"]!, Age: row["Age"]!, player_type: row["player_type"]!, mlbamid: row["mlbamid"]!,
      compound_id: `${row["PlayerId"]}_${row["player_type"]}`,
      display_name: duplicateNames.has(row["Name"]!) ? `${row["Name"]} (Hitter)` : row["Name"]!,
      position_detail: "Hitter",
      position: null,
    })),
    ...pitchers.rows.map((row) => ({
      Name: row["Name"]!, PlayerId: row["PlayerId"]!, Age: row["Age"]!, player_type: row["player_type"]!, mlbamid: row["mlbamid"]!,
      compound_id: `${row["PlayerId"]}_${row["player_type"]}`,
      display_name: duplicateNames.has(row["Name"]!) ? `${row["Name"]} (${row["position"]})` : row["Name"]!,
      position_detail: row["position"]!,
      position: row["position"]!,
    })),
  ];
  // arrange(Name): stable, by code point (dplyr's C-locale default), missing names last.
  const sorted = [...rows].sort((a, b) => {
    const left = a["Name"] as string | null;
    const right = b["Name"] as string | null;
    if (left === right) return 0;
    if (left === null) return 1;
    if (right === null) return -1;
    return compareCodepoints(left, right);
  });
  return { columns: [...PLAYER_LOOKUP_COLUMNS], rows: sorted };
}

export function buildServedPlayerInventory(lockedHitters: Table, lockedPitchers: Table, lockedLookup: Table): Table {
  const pick = (row: Row, source: string): Row => ({
    PlayerId: row["PlayerId"] ?? null, player_type: row["player_type"] ?? null, Name: row["Name"] ?? null, Age: row["Age"] ?? null,
    mlbamid: row["mlbamid"] ?? null, locked_source_file: source,
  });
  const right = {
    columns: ["PlayerId", "player_type", "Name", "Age", "mlbamid", "locked_source_file"],
    rows: [
      ...lockedHitters.rows.map((row) => pick(row, "full_stats_hitters.csv")),
      ...lockedPitchers.rows.map((row) => pick(row, "full_stats_pitchers.csv")),
    ],
  };
  const left = {
    columns: ["PlayerId", "player_type", "display_name", "compound_id", "position_detail"],
    rows: lockedLookup.rows.map((row) => ({
      PlayerId: row["PlayerId"] ?? null, player_type: row["player_type"] ?? null, display_name: row["display_name"] ?? null,
      compound_id: row["compound_id"] ?? null, position_detail: row["position_detail"] ?? null,
    })),
  };
  const joined = leftJoin(left, right, ["PlayerId", "player_type"]);
  const counts = new Map<string, number>();
  for (const row of joined.rows) {
    const key = JSON.stringify([row["PlayerId"], row["player_type"]]);
    const next = (counts.get(key) ?? 0) + 1;
    counts.set(key, next);
    row["locked_row_identity"] = next;
  }
  return selectColumns(joined.rows, SERVED_INVENTORY_COLUMNS);
}

const rowKey = (row: Row): string => `${row["PlayerId"] ?? "NA"}_${row["player_type"] ?? "NA"}`;

function isNumericColumn(rows: Rows, column: string): boolean {
  const present = rows.map((row) => row[column] ?? null).filter((value) => value !== null);
  return present.length > 0 && present.every((value) => typeof value === "number");
}

export type NumericMismatch = { row_key: string; field: string; locked_value: Num; candidate_value: Num; difference: Num };

/** Numeric columns the two files share, compared row by row (matched on PlayerId + player_type) within a tolerance. */
export function compareNumericParity(locked: Table, candidate: Table, tolerance = 0.001): NumericMismatch[] {
  const excluded = new Set([".row_key", "PlayerId", "player_type", "Name", "mlbamid", "position", "Age"]);
  const commonFields = intersect(locked.columns, candidate.columns).filter((column) => !excluded.has(column));

  const candidateIndex = new Map<string, Row[]>();
  for (const row of candidate.rows) {
    const bucket = candidateIndex.get(rowKey(row));
    if (bucket) bucket.push(row);
    else candidateIndex.set(rowKey(row), [row]);
  }
  const pairs: { key: string; locked: Row; candidate: Row }[] = [];
  for (const lockedRow of locked.rows) {
    for (const candidateRow of candidateIndex.get(rowKey(lockedRow)) ?? []) pairs.push({ key: rowKey(lockedRow), locked: lockedRow, candidate: candidateRow });
  }

  const mismatches: NumericMismatch[] = [];
  for (const field of commonFields) {
    // A column is numeric when it is in the locked file (guessed from its values) and in the candidate (known from the contract).
    const candidateNumeric = !TEXT_OUTPUT_COLUMNS.has(field) && candidate.rows.every((row) => row[field] === null || typeof row[field] === "number");
    if (!isNumericColumn(locked.rows, field) || !candidateNumeric) continue;
    for (const pair of pairs) {
      const lockedValue = num(pair.locked, field);
      const candidateValue = num(pair.candidate, field);
      const difference = lockedValue === null || candidateValue === null ? null : Math.abs(lockedValue - candidateValue);
      const bad = (lockedValue === null) !== (candidateValue === null) || (difference !== null && difference > tolerance);
      if (bad) {
        mismatches.push({
          row_key: pair.key, field, locked_value: lockedValue, candidate_value: candidateValue, difference: sub(candidateValue, lockedValue),
        });
      }
    }
  }
  return mismatches;
}

export type ParityReport = { status: "pass" | "fail"; failures: string[]; json: Json };

/**
 * Compares the freshly built candidate files with the files currently in production. When the data-through date has not moved,
 * the candidate must match production exactly (same players, same stats); when it has moved the stats are expected to differ,
 * so only the integrity checks apply (unique IDs, every served player has an MLB id). `extraFailures` carries failures from
 * other checks (see checkOutputSanity).
 *
 * (The R version also took --accept-source-migration for the one run that switched data sources; that run is done and the flag
 * is gone. The report still carries `source_migration: false` and an empty `warnings` list so its shape is unchanged.)
 */
export function validateMigrationParity(
  lockedHitters: Table, lockedPitchers: Table, lockedLookup: Table,
  candidateHitters: Table, candidatePitchers: Table, candidateLookup: Table,
  lockedDataThroughDate: string | null, candidateDataThroughDate: string, extraFailures: string[] = [],
): ParityReport {
  const lockedAll = [...lockedHitters.rows, ...lockedPitchers.rows];
  const candidateAll = [...candidateHitters.rows, ...candidatePitchers.rows];
  const lockedKeys = lockedAll.map(rowKey);
  const candidateKeys = candidateAll.map(rowKey);
  const sameCutoff = lockedDataThroughDate !== null && lockedDataThroughDate === candidateDataThroughDate;

  const missingPlayers = setdiff(lockedKeys, candidateKeys);
  const addedPlayers = setdiff(candidateKeys, lockedKeys);
  const missingMlbamids = candidateAll.filter((row) => row["mlbamid"] === null || row["mlbamid"] === undefined || row["mlbamid"] === "");
  const compoundCounts = new Map<string, number>();
  for (const row of candidateLookup.rows) {
    const id = String(row["compound_id"] ?? "NA");
    compoundCounts.set(id, (compoundCounts.get(id) ?? 0) + 1);
  }
  const duplicateCompoundIds = [...compoundCounts.entries()].filter(([, n]) => n > 1).sort(([a], [b]) => compareCodepoints(a, b));

  const lockedCompounds = lockedLookup.rows.map((row) => row["compound_id"] as string);
  const candidateCompounds = candidateLookup.rows.map((row) => row["compound_id"] as string);
  const missingCompounds = setdiff(lockedCompounds, candidateCompounds);
  const addedCompounds = setdiff(candidateCompounds, lockedCompounds);

  const statMismatches = sameCutoff
    ? [...compareNumericParity(lockedHitters, candidateHitters), ...compareNumericParity(lockedPitchers, candidatePitchers)]
    : [];

  const failures: string[] = [];
  if (sameCutoff && lockedHitters.rows.length !== candidateHitters.rows.length) failures.push("hitter row count changed");
  if (sameCutoff && lockedPitchers.rows.length !== candidatePitchers.rows.length) failures.push("pitcher row count changed");
  if (sameCutoff && missingPlayers.length > 0) failures.push("locked players missing from candidates");
  if (sameCutoff && addedPlayers.length > 0) failures.push("candidate contains non-locked players");
  if (sameCutoff && (missingCompounds.length > 0 || addedCompounds.length > 0)) failures.push("lookup compound IDs changed");
  if (duplicateCompoundIds.length > 0) failures.push("duplicate compound IDs");
  if (missingMlbamids.length > 0) failures.push("served players missing mlbamid");
  if (sameCutoff && statMismatches.length > 0) failures.push("stat parity mismatches");
  failures.push(...extraFailures);

  const status = failures.length > 0 ? "fail" : "pass";
  const json: Json = {
    status,
    locked_data_through_date: lockedDataThroughDate,
    candidate_data_through_date: candidateDataThroughDate,
    same_cutoff: sameCutoff,
    source_migration: false,
    row_counts: {
      locked_hitters: lockedHitters.rows.length,
      candidate_hitters: candidateHitters.rows.length,
      locked_pitchers: lockedPitchers.rows.length,
      candidate_pitchers: candidatePitchers.rows.length,
      locked_lookup: lockedLookup.rows.length,
      candidate_lookup: candidateLookup.rows.length,
    },
    missing_players: rVector(missingPlayers),
    added_players: rVector(addedPlayers),
    missing_lookup_compound_ids: rVector(missingCompounds),
    added_lookup_compound_ids: rVector(addedCompounds),
    duplicate_compound_ids: duplicateCompoundIds.map(([compound_id, n]) => ({ compound_id, n })),
    missing_mlbamids: missingMlbamids.map((row) => ({
      PlayerId: (row["PlayerId"] ?? null) as Json, player_type: (row["player_type"] ?? null) as Json, Name: (row["Name"] ?? null) as Json,
    })),
    stat_mismatches: statMismatches.map((m) => ({ ...m })),
    warnings: [],
    failures: rVector(failures),
  };
  return { status, failures, json };
}

// =============================================================================
// SANITY CHECKS
// =============================================================================
// Cheap checks on the candidate output that name the source when a feed has quietly gone empty. Returns failure messages (empty
// when fine); validateMigrationParity turns them into a failed run, which leaves the published CSVs alone.

export const REQUIRED_OUTPUT_SOURCES: Record<"hitters" | "pitchers", Record<string, string>> = {
  hitters: {
    Age: "MLB stats=season", PA_cur: "MLB stats=season", wOBA_cur: "MLB stats=sabermetrics", xwOBA_cur: "Savant expected_statistics",
    Barrel_pct_cur: "Savant statcast", AVG_cur: "MLB stats=season",
  },
  pitchers: {
    Age: "MLB stats=season", tbf: "MLB stats=season", era_cur: "MLB stats=season", fip_cur: "MLB stats=season (computed)",
    xera_cur: "Savant expected_statistics", barrel_percent_cur: "Savant custom", o_swing_percent_cur: "Savant custom",
    whiff_percent_cur: "Savant custom", babip_cur: "MLB stats=season (computed)", lob_percent_cur: "MLB stats=season (computed)",
  },
};

export function checkOutputSanity(
  hitters: Table, pitchers: Table, lockedHitters: Table, lockedPitchers: Table,
  lockedDataThroughDate: string | null, currentYear: number,
): string[] {
  const failures: string[] = [];
  const groups = { hitters, pitchers } as const;
  const locked = { hitters: lockedHitters, pitchers: lockedPitchers } as const;
  const names = ["hitters", "pitchers"] as const;

  for (const group of names) {
    const table = groups[group];
    const singular = group.replace(/s$/, "");
    if (table.rows.length === 0) {
      failures.push(`no ${group} in the new output`);
      continue;
    }
    for (const [column, source] of Object.entries(REQUIRED_OUTPUT_SOURCES[group])) {
      if (!table.columns.includes(column) || table.rows.every((row) => row[column] === null || row[column] === undefined)) {
        failures.push(`${column} is empty for every ${singular} (source: ${source})`);
      }
    }
  }

  // Within one season the player lists only grow, so a smaller list means a feed returned too little. A new season starts small, so
  // the comparison only applies when the previous output is from the same season.
  const sameSeason = lockedDataThroughDate !== null && lockedDataThroughDate.slice(0, 4) === String(currentYear);
  if (sameSeason) {
    const minimumRatio = 0.98;
    for (const group of names) {
      const now = groups[group].rows.length;
      const before = locked[group].rows.length;
      if (now > 0 && before > 0 && now < minimumRatio * before) {
        failures.push(`the ${group.replace(/s$/, "")} list shrank from ${before} to ${now} rows within the ${currentYear} season (a source returned too little)`);
      }
    }
  }
  return failures;
}
