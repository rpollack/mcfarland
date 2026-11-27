// The fetch layer and the season tables, run against small fixtures in the shape of the real responses. No test touches the network.
// A port of tests/testthat/test_data_sources.R.

import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import {
  ANY_SEASON_MIN_ROWS, baselineFile, buildBaselineSeason, buildSeasonStats, classifyPitcherRole, computeBabip, computeFip, computeLobPercent,
  fetchMlbHittingSeason, fetchMlbPitchingSeason, fetchSavantHitterBarrels, fetchSavantHitterXwoba, fetchSavantPitcherCustom,
  fetchSavantPitcherXera, fetchText, fipConstant, FULL_SEASON_MIN_ROWS, parseMlbInnings, parseMlbStats, parseSavantCsv, withRetry,
  writeBaselineSeason,
} from "../dataSources.ts";
import { readCsvTable } from "../format.ts";
import { requireColumns } from "../helpers.ts";
import { fixtureHttp, fixtureText, makeTempDir } from "./support.ts";

const quiet = () => {};
const byId = (rows: { [key: string]: unknown }[], id: string) => rows.find((row) => row["mlbamid"] === id);

describe("helpers", () => {
  it("reads MLB innings as thirds", () => {
    expect(["207.0", "58.2", "0.1", "1"].map(parseMlbInnings)).toEqual([207, 58 + 2 / 3, 1 / 3, 1]);
    expect(parseMlbInnings(null)).toBe(0);
    expect(parseMlbInnings(12.2)).toBe(12 + 2 / 3);
  });

  it("retries a transient failure and names the source when it persists", async () => {
    let calls = 0;
    const flaky = async () => {
      calls++;
      if (calls < 3) throw new Error("status 503");
      return "ok";
    };
    await expect(withRetry(flaky, "Savant test", { waitSeconds: 0, log: quiet })).resolves.toBe("ok");
    expect(calls).toBe(3);
    await expect(
      fetchText("https://example.invalid", "Savant test", { httpGet: async () => { throw new Error("status 403"); }, attempts: 1, log: quiet }),
    ).rejects.toThrow("Savant test failed after 1 attempts: status 403");
  });

  it("waits 10s then 20s between attempts", async () => {
    const waits: number[] = [];
    await expect(
      withRetry(async () => { throw new Error("boom"); }, "x", { log: quiet, sleep: async (ms) => { waits.push(ms); } }),
    ).rejects.toThrow("failed after 3 attempts");
    expect(waits).toEqual([10_000, 20_000]);
  });

  it("requireColumns names every missing column", () => {
    expect(() => requireColumns(["a"], ["a", "b", "c"], "Some feed")).toThrow("Some feed is missing expected columns: b, c");
  });

  it("classifies roles with R's three-valued logic", () => {
    expect(classifyPitcherRole(34, 34, 207)).toBe("SP");
    expect(classifyPitcherRole(0, 62, 58.67)).toBe("RP");
    expect(classifyPitcherRole(5, 10, 30)).toBe("SP"); // exactly half the games started
    expect(classifyPitcherRole(0, 10, 41)).toBe("SP"); // more than four innings an appearance
    expect(classifyPitcherRole(2, 0, 0)).toBe("SP"); // 2/0 is Inf
    expect(classifyPitcherRole(0, 0, 0)).toBeNull(); // 0/0 is NaN: R's NA
    expect(classifyPitcherRole(0, 0, 50)).toBe("SP"); // NA | TRUE is TRUE
  });
});

describe("MLB Stats API", () => {
  it("keeps only major-league rows and requires the columns it uses", async () => {
    const parsed = parseMlbStats(fixtureText("mlb_hitting_season.json"), "hitting");
    expect(parsed.rows).toHaveLength(5);
    expect(parsed.rows.map((row) => String(row["player.id"]))).not.toContain("999001");

    const hitting = await fetchMlbHittingSeason(2025, { httpGet: fixtureHttp() });
    expect(hitting).toHaveLength(5);
    expect(byId(hitting, "514888")?.["Name"]).toBe("Jose Altuve");
    expect(byId(hitting, "592450")?.["Age"]).toBe(33);

    expect(() => parseMlbStats('{"stats":[{"splits":[]}]}', "hitting")).toThrow("returned no rows");
    expect(() => parseMlbStats(fixtureText("mlb_hitting_season.json"), "hitting", 10)).toThrow("fewer than the 10 expected");
    const noAge = fixtureText("mlb_hitting_season.json").replaceAll('"age"', '"ageX"');
    await expect(fetchMlbHittingSeason(2025, { httpGet: async () => noAge })).rejects.toThrow("missing expected columns: stat.age");
  });

  it("stops the run when a player is repeated", () => {
    const repeated = fixtureText("mlb_hitting_season.json").replace('"id": 607067', '"id": 592450');
    expect(() => parseMlbStats(repeated, "hitting")).toThrow("more than one row for a player");
    expect(parseMlbStats(repeated, "hitting", 1, false).rows).toHaveLength(5);
  });

  it("treats a field a row lacks as missing, like fromJSON(flatten = TRUE)", async () => {
    const edge = await fetchMlbHittingSeason(2025, { httpGet: fixtureHttp("fixtures-edge") });
    const missing = byId(edge, "700006");
    expect(missing?.["H"]).toBeNull();
    expect(missing?.["Age"]).toBeNull();
    expect(byId(edge, "700009")?.["primary_position"]).toBeNull();
  });

  it("asks for the season, hydrate and limit the R code asked for", async () => {
    const requested: string[] = [];
    await fetchMlbPitchingSeason(2025, { httpGet: fixtureHttp("fixtures", requested) });
    expect(requested).toEqual([
      "https://statsapi.mlb.com/api/v1/stats?stats=season&group=pitching&playerPool=ALL&season=2025&hydrate=person(currentTeam,primaryPosition)&limit=10000",
    ]);
  });
});

describe("Baseball Savant", () => {
  it("reads a CSV past its byte-order mark with ids as text", () => {
    expect(fixtureText("savant_expected_batter.csv").startsWith("﻿")).toBe(true);
    const parsed = parseSavantCsv(fixtureText("savant_expected_batter.csv"), ["est_woba"], "Savant test");
    expect(parsed.columns).toContain("last_name, first_name");
    expect(parsed.rows.map((row) => row["mlbamid"])).toEqual(["592450", "660271", "514888", "607067"]);
  });

  it("fails loudly on a response that is empty, HTML, thin, repeated or reshaped", () => {
    const csv = fixtureText("savant_expected_batter.csv");
    expect(() => parseSavantCsv("", ["est_woba"], "Savant test")).toThrow("Savant test returned an empty response");
    expect(() => parseSavantCsv("<!DOCTYPE html><html><body>Just a moment...</body></html>", ["est_woba"], "Savant test")).toThrow("returned HTML instead of CSV");
    expect(() => parseSavantCsv(csv, ["xera"], "Savant test")).toThrow("Savant test is missing expected columns: xera");
    expect(() => parseSavantCsv(csv, ["est_woba"], "Savant test", 50)).toThrow("returned 4 rows, fewer than the 50 expected");
    expect(() => parseSavantCsv(csv.replace('"660271"', '"592450"'), ["est_woba"], "Savant test")).toThrow("missing or repeated player_id");
    expect(() => parseSavantCsv("player_id\n", ["est_woba"], "Savant test")).toThrow(/missing expected columns|fewer than/);
  });

  it("asks each fetcher for the right leaderboard, season and player type", async () => {
    const requested: string[] = [];
    const httpGet = fixtureHttp("fixtures", requested);
    await fetchSavantHitterXwoba(2025, { httpGet });
    await fetchSavantHitterBarrels(2025, { httpGet });
    await fetchSavantPitcherXera(2025, { httpGet });
    await fetchSavantPitcherCustom(2025, { httpGet });
    expect(requested[0]).toContain("leaderboard/expected_statistics?type=batter&year=2025");
    expect(requested[1]).toContain("leaderboard/statcast?type=batter&year=2025");
    expect(requested[2]).toContain("leaderboard/expected_statistics?type=pitcher&year=2025");
    expect(requested[3]).toContain("leaderboard/custom?year=2025&type=pitcher");
    expect(requested[3]).toContain("barrel_batted_rate,oz_swing_percent,whiff_percent");
    expect(requested.every((url) => url.endsWith("&csv=true"))).toBe(true);
  });
});

describe("computed fields", () => {
  it("counts intentional walks in FIP and makes league FIP equal league ERA", async () => {
    const pitching = await fetchMlbPitchingSeason(2025, { httpGet: fixtureHttp() });
    const constant = fipConstant(pitching)!;
    // League (position players included): 313 2/3 IP, 101 ER, 24 HR, 81 BB, 9 HBP, 354 K.
    expect(constant).toBeCloseTo((9 * 101 + 126) / (313 + 2 / 3), 9);

    const num = (row: { [key: string]: unknown }, key: string) => row[key] as number;
    const fips = pitching.map((p) => computeFip(num(p, "hr"), num(p, "bb"), num(p, "hbp"), num(p, "so"), num(p, "ip"), constant));
    let weighted = 0;
    let innings = 0;
    pitching.forEach((p, index) => {
      innings += num(p, "ip");
      if (num(p, "ip") > 0) weighted += fips[index]! * num(p, "ip");
    });
    const leagueEra = (9 * pitching.reduce((sum, p) => sum + num(p, "er"), 0)) / innings;
    expect(weighted / innings).toBeCloseTo(leagueEra, 9);
    expect(fips[pitching.findIndex((p) => num(p, "ip") === 0)]).toBeNull();
    // Logan Webb: (13 * 14 + 3 * (46 + 5) - 2 * 224) / 207 + constant
    expect(fips[pitching.findIndex((p) => p["mlbamid"] === "657277")]).toBeCloseTo(-113 / 207 + constant, 9);
  });

  it("computes BABIP and LOB% with the standard formulas, LOB% clamped to 0-100", () => {
    expect(computeBabip(190, 14, 770, 224, 5)).toBe(176 / 537);
    expect(computeBabip(0, 0, 0, 0, 0)).toBeNull();

    expect(computeLobPercent(190, 46, 5, 14, 80)).toBeCloseTo((100 * 161) / 221.4, 12);
    // One inning, two home runs: the formula gives 500%, which cannot be a rate, so it is clamped.
    expect(computeLobPercent(3, 0, 0, 2, 2)).toBe(100);
    // More runs than baserunners cannot go below zero.
    expect(computeLobPercent(4, 0, 0, 0, 6)).toBe(0);
    // Nobody could have scored: no baserunners, or a lone home run.
    expect(computeLobPercent(0, 0, 0, 0, 0)).toBeNull();
    expect(computeLobPercent(1, 0, 0, 1, 1)).toBeNull();
  });
});

describe("season tables", () => {
  const season = () => buildSeasonStats(2025, { minRows: ANY_SEASON_MIN_ROWS, httpGet: fixtureHttp(), log: quiet });

  it("drops batting pitchers and zero-PA players but keeps the two-way player", async () => {
    const { hitters } = await season();
    expect(hitters.map((row) => row["mlbamid"]).sort()).toEqual(["514888", "592450", "660271"]);
    expect(hitters.map((row) => row["mlbamid"])).not.toContain("607067");
    expect(hitters.map((row) => row["mlbamid"])).not.toContain("680911");
    const judge = byId(hitters, "592450")!;
    expect(judge["PA"]).toBe(679);
    expect(judge["Barrels"]).toBe(84);
    expect(judge["wOBA"]).toBe(0.462771);
    expect(judge["xwOBA"]).toBe(0.449);
    expect(Object.keys(judge)).toEqual(["mlbamid", "Name", "Age", "G", "AB", "PA", "H", "2B", "3B", "HR", "BB", "HBP", "SF", "SO", "TB", "Barrels", "wOBA", "xwOBA"]);
  });

  it("gives a hitter with no Savant batted-ball row 0 barrels but keeps the other values", async () => {
    const altuve = byId((await season()).hitters, "514888")!;
    expect(altuve["Name"]).toBe("Jose Altuve");
    expect(altuve["Barrels"]).toBe(0);
    expect(altuve["xwOBA"]).toBe(0.318);
  });

  it("classifies pitcher roles, computes rates and leaves out position players", async () => {
    const { pitchers } = await season();
    expect(pitchers.map((row) => row["mlbamid"]).sort()).toEqual(["657277", "660271", "695243"]);
    expect(pitchers.map((row) => row["mlbamid"])).not.toContain("555001");

    const webb = byId(pitchers, "657277")!;
    expect(webb["position"]).toBe("SP");
    expect(webb["age"]).toBe(28);
    expect(webb["ip"]).toBe(207);
    expect(webb["x_era"]).toBe(3.58);
    expect(webb["barrel_percent"]).toBe(7.1);
    expect(webb["o_swing_percent"]).toBe(31.5);
    expect(webb["whiff_percent"]).toBe(24.4);
    expect(webb["babip"]).toBe(0.327747); // round(176 / 537, 6)

    const miller = byId(pitchers, "695243")!;
    expect(miller["position"]).toBe("RP");
    expect(miller["ip"]).toBe(58.666667);

    // A position player who pitched is not a pitcher (the FIP still counts in the league constant); Ohtani (TWP) is.
    expect(pitchers.map((row) => row["mlbamid"])).not.toContain("666126");
    expect(pitchers.map((row) => row["mlbamid"])).toContain("660271");
  });

  it("refuses a thin response for a full season and accepts it while a season is in progress", async () => {
    await expect(buildSeasonStats(2025, { minRows: FULL_SEASON_MIN_ROWS, httpGet: fixtureHttp(), log: quiet })).rejects.toThrow(/fewer than the \d+ expected/);
    await expect(season()).resolves.toBeDefined();
  });

  it("sorts the season tables by numeric id and the edge fixtures keep their hazards", async () => {
    const { hitters, pitchers } = await buildSeasonStats(2025, { minRows: ANY_SEASON_MIN_ROWS, httpGet: fixtureHttp("fixtures-edge"), log: quiet });
    expect(hitters[0]!["mlbamid"]).toBe("54321"); // numeric order puts the 5-digit id first
    expect(byId(hitters, "700001")?.["Name"]).toBe("Orsted Lukasz Duric Jr.");
    expect(byId(hitters, "700002")?.["wOBA"]).toBeNull();
    expect(byId(hitters, "700003")?.["wOBA"]).toBeNull(); // "-.--"
    expect(byId(pitchers, "800004")?.["ip"]).toBe(12.666667);
    expect(byId(pitchers, "800002")?.["fip"]).toBeNull();
    expect(byId(pitchers, "800001")?.["babip"]).toBe(0.0001);
  });
});

describe("baseline files", () => {
  it("names them by kind and year", () => {
    expect(baselineFile("hitters", 2025, "elsewhere")).toBe(join("elsewhere", "baseline-hitters-2025.csv"));
    expect(baselineFile("pitchers", 2023, join("data", "baselines"))).toBe(join("data", "baselines", "baseline-pitchers-2023.csv"));
  });

  it("writes a baseline season once and replaces it only with force", async () => {
    const dir = makeTempDir();
    const stats = await buildSeasonStats(2025, { minRows: ANY_SEASON_MIN_ROWS, httpGet: fixtureHttp(), log: quiet });
    const files = writeBaselineSeason(2025, stats, dir, false, quiet);

    const written = readCsvTable(readFileSync(files.hitters, "utf8"), ["mlbamid"]);
    expect(written.rows).toHaveLength(3);
    expect(written.columns).toEqual(Object.keys(stats.hitters[0]!));

    const { writeFileSync } = await import("node:fs");
    writeFileSync(files.pitchers, "marker\n");
    expect(() => writeBaselineSeason(2025, stats, dir, false, quiet)).toThrow("Refusing to overwrite");
    await expect(buildBaselineSeason(2025, { dir, httpGet: fixtureHttp(), log: quiet })).rejects.toThrow("Refusing to overwrite");
    expect(readFileSync(files.pitchers, "utf8")).toBe("marker\n");

    writeBaselineSeason(2025, stats, dir, true, quiet);
    expect(readCsvTable(readFileSync(files.pitchers, "utf8")).rows).toHaveLength(3);
  });
});
