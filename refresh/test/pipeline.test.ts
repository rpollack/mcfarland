// Baselines, frozen player ids, the published-file contract, the gates, and the edges of the arithmetic. No test touches the network.
// A port of tests/testthat/test_pipeline.R (the end-to-end runs are in golden.test.ts).

import { createHash } from "node:crypto";
import { existsSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { CURRENT_YEAR } from "../config.ts";
import { ANY_SEASON_MIN_ROWS, baselineFile, buildSeasonStats, writeBaselineSeason } from "../dataSources.ts";
import { dataThroughDate, readCsvTable, type Row } from "../format.ts";
import { groupBy, leftJoin, weightedMean } from "../helpers.ts";
import {
  addPlayerIds, checkOutputSanity, checkPlayerIdCollisions, readHitterBaselineSeasons, readPitcherBaselineSeasons, readPlayerIdMap,
  summarizeHitterMetrics, summarizePitcherBaselineMetrics, validateMigrationParity,
} from "../pipeline.ts";
import { baselineSeasonsFor, HITTER_OUTPUT_COLUMNS, PITCHER_OUTPUT_COLUMNS, type Table } from "../types.ts";
import { fixtureHttp, makeTempDir } from "./support.ts";

const REPO_ROOT = new URL("../../", import.meta.url).pathname;
const quiet = () => {};

// --- baselines ----------------------------------------------------------------

describe("baselines", () => {
  it("weights pitcher baseline metrics by season and keeps percents as percents", () => {
    const seasons: Row[] = [
      { mlbamid: "1", position: "SP", w: 1, er: 10, ip: 90, so: 80, bb: 20, tbf: 400, x_era: 3, fip: 3.2, barrel_percent: 7, o_swing_percent: 30, babip: 0.29, lob_percent: 75, whiff_percent: 24 },
      { mlbamid: "1", position: "SP", w: 3, er: 20, ip: 100, so: 120, bb: 30, tbf: 420, x_era: 4, fip: 3.8, barrel_percent: 9, o_swing_percent: 32, babip: 0.3, lob_percent: 80, whiff_percent: 26 },
    ];
    const out = summarizePitcherBaselineMetrics(seasons);
    expect(out["era"]).toBeCloseTo((9 * (10 * 1 + 20 * 3)) / (90 * 1 + 100 * 3), 12);
    expect(out["k_percent"]).toBeCloseTo((100 * (80 * 1 + 120 * 3)) / (400 * 1 + 420 * 3), 12);
    // Weighted by batters faced times the season weight; the values are already percents, so no x100.
    expect(out["whiff_percent"]).toBeCloseTo((24 * 400 * 1 + 26 * 420 * 3) / (400 * 1 + 420 * 3), 12);
    expect(out["whiff_percent"] as number).toBeGreaterThan(20);
    expect(out["whiff_percent"] as number).toBeLessThan(30);
    expect(out["barrel_percent"]).toBeCloseTo((7 * 400 * 1 + 9 * 420 * 3) / (400 * 1 + 420 * 3), 12);
    expect(Object.keys(out)).not.toContain("csw_percent");
  });

  it("weights the three seasons before the current one 1, 3, 5", () => {
    expect(baselineSeasonsFor(2027)).toEqual([{ year: 2024, weight: 1 }, { year: 2025, weight: 3 }, { year: 2026, weight: 5 }]);
  });

  it("refuses a baseline file in another shape by name, and a missing one by the command that builds it", async () => {
    const dir = makeTempDir();
    const seasons = baselineSeasonsFor(2026);
    const stats = await buildSeasonStats(2025, { minRows: ANY_SEASON_MIN_ROWS, httpGet: fixtureHttp(), log: quiet });
    for (const { year } of seasons) writeBaselineSeason(year, stats, dir, true, quiet);
    expect(readHitterBaselineSeasons(seasons, dir)).toHaveLength(9);
    expect([...new Set(readPitcherBaselineSeasons(seasons, dir).map((row) => row["w"]))]).toEqual([1, 3, 5]);

    const file = baselineFile("pitchers", 2025, dir);
    writeFileSync(file, readFileSync(file, "utf8").replace("whiff_percent", "csw_percent"));
    expect(() => readPitcherBaselineSeasons(seasons, dir)).toThrow("baseline-pitchers-2025.csv is missing expected columns: whiff_percent");

    rmSync(baselineFile("hitters", 2024, dir));
    expect(() => readHitterBaselineSeasons(seasons, dir)).toThrow("baseline-hitters-2024.csv is missing; run npm run refresh:baseline -- 2024");
  });

  it("has the baseline files for the three seasons before CURRENT_YEAR committed", () => {
    // Bumping CURRENT_YEAR in refresh/config.ts for a new season needs last season's files first
    // (npm run refresh:baseline -- <year>); this fails the bump's PR if they are missing.
    for (const { year } of baselineSeasonsFor(CURRENT_YEAR)) {
      for (const kind of ["hitters", "pitchers"] as const) {
        expect(existsSync(baselineFile(kind, year, join(REPO_ROOT, "data", "baselines"))), `${kind} ${year}`).toBe(true);
      }
    }
  });

  it("sums PA plainly but weights the rate numerators and denominators", () => {
    const rows: Row[] = [
      { H: 100, AB: 400, PA: 450, BB: 40, HBP: 5, SF: 5, TB: 160, SO: 90, HR: 10, Barrels: 20, wOBA: 0.3, xwOBA: 0.31, w: 1 },
      { H: 150, AB: 500, PA: 560, BB: 50, HBP: 5, SF: 5, TB: 250, SO: 100, HR: 20, Barrels: 30, wOBA: 0.35, xwOBA: 0.34, w: 3 },
    ];
    const out = summarizeHitterMetrics(rows);
    expect(out["PA"]).toBe(1010);
    expect(out["AVG"]).toBe((100 * 1 + 150 * 3) / (400 * 1 + 500 * 3));
    expect(out["wOBA"]).toBeCloseTo((0.3 * 450 * 1 + 0.35 * 560 * 3) / (450 * 1 + 560 * 3), 12);
  });

  it("lets one missing count poison a player's baseline when na.rm is off, but not the league's", () => {
    const rows: Row[] = [
      { H: null, AB: 400, PA: 450, BB: 40, HBP: 5, SF: 5, TB: 160, SO: 90, HR: 10, Barrels: 20, wOBA: 0.3, xwOBA: 0.31, w: 1 },
      { H: 150, AB: 500, PA: 560, BB: 50, HBP: 5, SF: 5, TB: 250, SO: 100, HR: 20, Barrels: 30, wOBA: 0.35, xwOBA: 0.34, w: 3 },
    ];
    expect(summarizeHitterMetrics(rows, false)["AVG"]).toBeNull();
    expect(summarizeHitterMetrics(rows, true)["AVG"]).toBe((150 * 3) / (400 * 1 + 500 * 3));
  });
});

// --- frozen player ids ---------------------------------------------------------

describe("frozen player ids", () => {
  const mapFile = join(REPO_ROOT, "data", "player_id_map.csv");

  it("is exactly as it was built and its two id ranges cannot overlap", () => {
    // Built once from the player_lookup.csv that was live when the pipeline left FanGraphs. Never regenerate it: doing so would change
    // every existing player's id (URLs, votes, trend rows, cached analyses, stars).
    expect(createHash("md5").update(readFileSync(mapFile)).digest("hex")).toBe("49d04183a2e4f47f0ba412549425a6e7");
    const idMap = readPlayerIdMap(mapFile);
    expect(idMap.size).toBe(1464);
    const playerIds = [...idMap.values()];
    expect(playerIds.every((id) => /^[0-9]+$/.test(id))).toBe(true);
    expect(Math.max(...playerIds.map(Number))).toBeLessThan(100000);
    expect(Math.min(...[...idMap.keys()].map(Number))).toBeGreaterThan(400000);
  });

  it("gives a mapped player the legacy id and a new player the mlbamid", () => {
    const idMap = new Map([["592450", "15640"], ["657277", "17995"]]);
    const players: Row[] = [{ mlbamid: "592450", Name: "a" }, { mlbamid: "660271", Name: "b" }, { mlbamid: "657277", Name: "c" }];
    addPlayerIds(players, idMap);
    expect(players.map((row) => row["PlayerId"])).toEqual(["15640", "660271", "17995"]);
    expect(players.map((row) => row["Name"])).toEqual(["a", "b", "c"]);
  });

  it("refuses a map that repeats a player or an id, and an id collision", () => {
    const dir = makeTempDir();
    const file = join(dir, "map.csv");
    writeFileSync(file, "mlbamid,PlayerId\n1,5\n2,5\n");
    expect(() => readPlayerIdMap(file)).toThrow("maps a player or an id more than once");

    const idMap = new Map([["592450", "15640"]]);
    expect(() => checkPlayerIdCollisions([{ mlbamid: "592450" }, { mlbamid: "660271" }], idMap)).not.toThrow();
    expect(() => checkPlayerIdCollisions([{ mlbamid: "15640" }], idMap)).toThrow("equal to a frozen legacy PlayerId: 15640");
  });

  it("is carried by the committed outputs: every mapped player's legacy id, every other player's mlbamid", () => {
    const idMap = readPlayerIdMap(mapFile);
    for (const file of ["full_stats_hitters.csv", "full_stats_pitchers.csv", "player_lookup.csv"]) {
      const published = readCsvTable(readFileSync(join(REPO_ROOT, "data", file), "utf8"), ["PlayerId", "mlbamid"]);
      const mapped = published.rows.filter((row) => idMap.has(row["mlbamid"] as string));
      expect(mapped.length, file).toBeGreaterThan(0);
      for (const row of mapped) expect(row["PlayerId"], file).toBe(idMap.get(row["mlbamid"] as string));
      for (const row of published.rows.filter((r) => !idMap.has(r["mlbamid"] as string))) expect(row["PlayerId"], file).toBe(row["mlbamid"]);
    }
  });
});

// --- the published files -------------------------------------------------------

describe("the published files", () => {
  it("have exactly the contracted columns", () => {
    const hitters = readCsvTable(readFileSync(join(REPO_ROOT, "data", "full_stats_hitters.csv"), "utf8"));
    const pitchers = readCsvTable(readFileSync(join(REPO_ROOT, "data", "full_stats_pitchers.csv"), "utf8"));
    expect(hitters.columns).toEqual(HITTER_OUTPUT_COLUMNS);
    expect(pitchers.columns).toEqual(PITCHER_OUTPUT_COLUMNS);
    expect(pitchers.columns.some((column) => /csw|x_fip|hr_fb|f_bv|c_tv|s_fv|ev90|barrels/.test(column))).toBe(false);
    for (const column of ["whiff_percent_cur", "whiff_percent_l3", "whiff_percent_diff", "whiff_percent_lg_adj_diff"]) {
      expect(pitchers.columns).toContain(column);
    }
  });
});

// --- the gates -----------------------------------------------------------------

const tinyOutput = (type: string, ids: string[], stat = 1): Table => ({
  columns: ["PlayerId", "mlbamid", "Name", "Age", "player_type", "stat_cur"],
  rows: ids.map((id) => ({ PlayerId: id, mlbamid: id, Name: `Player ${id}`, Age: 30, player_type: type, stat_cur: stat })),
});
const tinyLookup = (...tables: Table[]): Table => ({
  columns: ["PlayerId", "mlbamid", "Name", "player_type", "compound_id"],
  rows: tables.flatMap((t) => t.rows).map((row) => ({
    PlayerId: row["PlayerId"]!, mlbamid: row["mlbamid"]!, Name: row["Name"]!, player_type: row["player_type"]!, compound_id: `${row["PlayerId"]}_${row["player_type"]}`,
  })),
});

describe("validateMigrationParity", () => {
  it("fails a changed player list or stat with the same data date, and checks integrity only once the date moved", () => {
    const lockedH = tinyOutput("hitter", ["1", "2", "3"]);
    const newH = tinyOutput("hitter", ["1", "2", "4"], 5);
    const p = tinyOutput("pitcher", ["10", "11"]);

    const strict = validateMigrationParity(lockedH, p, tinyLookup(lockedH, p), newH, p, tinyLookup(newH, p), "2026-11-16", "2026-11-16");
    expect(strict.status).toBe("fail");
    expect(strict.failures).toEqual(expect.arrayContaining(["locked players missing from candidates", "candidate contains non-locked players", "stat parity mismatches"]));

    // The new files, a day later, are checked for integrity only.
    const later = validateMigrationParity(newH, p, tinyLookup(newH, p), newH, p, tinyLookup(newH, p), "2026-11-16", "2026-11-17");
    expect(later.status).toBe("pass");
  });

  it("catches duplicate compound ids, missing mlbamids and sanity failures", () => {
    const p = tinyOutput("pitcher", ["10", "11"]);
    const h = tinyOutput("hitter", ["1", "2"]);
    const duplicated = tinyLookup(h, p, { columns: h.columns, rows: [h.rows[0]!] });
    expect(validateMigrationParity(h, p, tinyLookup(h, p), h, p, duplicated, "2026-11-16", "2026-11-16").failures).toContain("duplicate compound IDs");

    const noId: Table = { columns: h.columns, rows: [h.rows[0]!, { ...h.rows[1]!, mlbamid: null }] };
    expect(validateMigrationParity(h, p, tinyLookup(h, p), noId, p, tinyLookup(h, p), "2026-11-16", "2026-11-16").failures).toContain("served players missing mlbamid");

    const failed = validateMigrationParity(h, p, tinyLookup(h, p), h, p, tinyLookup(h, p), "2026-11-16", "2026-11-16", ["a feed went empty"]);
    expect(failed.status).toBe("fail");
    expect(failed.failures).toEqual(["a feed went empty"]);
  });

  it("compares numeric columns within 0.001 and reports the rest by name", () => {
    const locked = tinyOutput("hitter", ["1", "2"]);
    const candidate: Table = { columns: locked.columns, rows: locked.rows.map((row, i) => ({ ...row, stat_cur: i === 0 ? 1.0009 : 1.002 })) };
    const p = tinyOutput("pitcher", ["10"]);
    const report = validateMigrationParity(locked, p, tinyLookup(locked, p), candidate, p, tinyLookup(candidate, p), "2026-11-16", "2026-11-16");
    expect(report.json).toMatchObject({ stat_mismatches: [{ row_key: "2_hitter", field: "stat_cur", locked_value: 1, candidate_value: 1.002 }] });
  });
});

describe("checkOutputSanity", () => {
  const fullHitters = (n = 1): Table => ({
    columns: ["Age", "PA_cur", "wOBA_cur", "xwOBA_cur", "Barrel_pct_cur", "AVG_cur"],
    rows: Array.from({ length: n }, () => ({ Age: 30, PA_cur: 100, wOBA_cur: 0.3, xwOBA_cur: 0.31, Barrel_pct_cur: 8, AVG_cur: 0.25 })),
  });
  const fullPitchers = (): Table => ({
    columns: ["Age", "tbf", "era_cur", "fip_cur", "xera_cur", "barrel_percent_cur", "o_swing_percent_cur", "whiff_percent_cur", "babip_cur", "lob_percent_cur"],
    rows: [{ Age: 30, tbf: 100, era_cur: 4, fip_cur: 4, xera_cur: 4, barrel_percent_cur: 8, o_swing_percent_cur: 30, whiff_percent_cur: 25, babip_cur: 0.3, lob_percent_cur: 72 }],
  });

  it("fails a source that went empty and names it", () => {
    expect(checkOutputSanity(fullHitters(), fullPitchers(), fullHitters(), fullPitchers(), "2026-11-16", 2026)).toEqual([]);

    const noWhiff: Table = { ...fullPitchers(), rows: fullPitchers().rows.map((row) => ({ ...row, whiff_percent_cur: null })) };
    expect(checkOutputSanity(fullHitters(), noWhiff, fullHitters(), fullPitchers(), "2026-11-16", 2026)).toEqual([
      "whiff_percent_cur is empty for every pitcher (source: Savant custom)",
    ]);

    const noXwoba: Table = { ...fullHitters(), rows: fullHitters().rows.map((row) => ({ ...row, xwOBA_cur: null })) };
    expect(checkOutputSanity(noXwoba, fullPitchers(), fullHitters(), fullPitchers(), "2026-11-16", 2026)[0]).toMatch(/xwOBA_cur is empty for every hitter \(source: Savant expected_statistics\)/);

    expect(checkOutputSanity(fullHitters(0), fullPitchers(), fullHitters(), fullPitchers(), "2026-11-16", 2026)[0]).toMatch(/no hitters in the new output/);
  });

  it("lets a player list stay or grow within a season but not shrink, while a new season starts small", () => {
    expect(checkOutputSanity(fullHitters(90), fullPitchers(), fullHitters(100), fullPitchers(), "2026-06-01", 2026)[0]).toMatch(
      /hitter list shrank from 100 to 90 rows within the 2026 season/,
    );
    expect(checkOutputSanity(fullHitters(99), fullPitchers(), fullHitters(100), fullPitchers(), "2026-06-01", 2026)).toEqual([]);
    // Last season's final files are much bigger than opening week; that is not a failure.
    expect(checkOutputSanity(fullHitters(10), fullPitchers(), fullHitters(100), fullPitchers(), "2026-10-01", 2027)).toEqual([]);
    expect(checkOutputSanity(fullHitters(10), fullPitchers(), fullHitters(100), fullPitchers(), null, 2026)).toEqual([]);
  });
});

// --- helpers with R semantics -------------------------------------------------

describe("R-flavoured helpers", () => {
  it("weightedMean drops missing values, ignores zero weights and returns missing when nothing is left", () => {
    expect(weightedMean([1, null, 3], [1, 5, 3])).toBe((1 * 1 + 3 * 3) / 4);
    expect(weightedMean([null, null], [1, 1])).toBeNull();
    expect(weightedMean([2, 4], [0, 0])).toBeNull(); // 0/0 is NaN in R
    expect(weightedMean([2, 4], [null, 1])).toBeNull(); // a missing weight on a kept value poisons the mean
  });

  it("groupBy orders groups by key in code-point order (dplyr), not by arrival or locale", () => {
    const rows: Row[] = [{ k: "99999" }, { k: "100000" }, { k: "Zed" }, { k: "apple" }, { k: null }];
    expect(groupBy(rows, ["k"]).map((group) => group.key[0])).toEqual(["100000", "99999", "Zed", "apple", null]);
  });

  it("leftJoin suffixes shared columns, repeats left rows per match and fills misses with missing", () => {
    const left = { columns: ["id", "x"], rows: [{ id: "a", x: 1 }, { id: "b", x: 2 }] };
    const right = { columns: ["id", "x", "y"], rows: [{ id: "a", x: 10, y: 5 }, { id: "a", x: 11, y: 6 }] };
    const joined = leftJoin(left, right, ["id"], ["_cur", "_l3"]);
    expect(joined.columns).toEqual(["id", "x_cur", "x_l3", "y"]);
    expect(joined.rows).toEqual([
      { id: "a", x_cur: 1, x_l3: 10, y: 5 }, { id: "a", x_cur: 1, x_l3: 11, y: 6 }, { id: "b", x_cur: 2, x_l3: null, y: null },
    ]);
  });
});

describe("the data-through date in a run", () => {
  it("is yesterday in Central time at the pinned clock", () => {
    expect(dataThroughDate(new Date("2026-11-26T15:00:00Z"))).toBe("2026-11-25");
  });
});

describe("review follow-ups", () => {
  it("refuses stray text in a numeric baseline column, as R's arithmetic would", async () => {
    const dir = makeTempDir();
    const seasons = baselineSeasonsFor(2026);
    const stats = await buildSeasonStats(2025, { minRows: ANY_SEASON_MIN_ROWS, httpGet: fixtureHttp(), log: quiet });
    for (const { year } of seasons) writeBaselineSeason(year, stats, dir, true, quiet);
    const file = baselineFile("hitters", 2025, dir);
    const lines = readFileSync(file, "utf8").split("\n");
    const header = lines[0]!.split(",");
    const cells = lines[1]!.split(",");
    cells[header.indexOf("H")] = "--";
    lines[1] = cells.join(",");
    writeFileSync(file, lines.join("\n"));
    expect(() => readHitterBaselineSeasons(seasons, dir)).toThrow('non-numeric text in column H: "--"');
  });
});
