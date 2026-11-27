import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { main, promoteCandidateOutputs, readDataThroughDate } from "../refreshData.ts";
import { dataPaths } from "../config.ts";
import { makeTempDir } from "./support.ts";

describe("command line", () => {
  it("asks for a year instead of running the daily refresh when build-baseline-season has none", async () => {
    await expect(main(["build-baseline-season"], { DATA_DIR: makeTempDir() })).rejects.toThrow('build-baseline-season needs a year, got ""');
    await expect(main(["build-baseline-season", "abc"], { DATA_DIR: makeTempDir() })).rejects.toThrow('got "abc"');
  });

  it("refuses to overwrite an existing baseline season without --force (before touching the network)", async () => {
    const dir = makeTempDir();
    mkdirSync(join(dir, "baselines"));
    writeFileSync(join(dir, "baselines", "baseline-hitters-2025.csv"), "x\n");
    await expect(main(["build-baseline-season", "2025"], { DATA_DIR: dir })).rejects.toThrow("Refusing to overwrite");
  });

  it("does not copy anything over production when the parity report failed", () => {
    const logs: string[] = [];
    const report = { status: "fail" as const, failures: ["a", "b"], json: null };
    expect(promoteCandidateOutputs(report, dataPaths(makeTempDir()), (line) => logs.push(line))).toBe(false);
    expect(logs).toEqual(["Migration parity failed; production CSVs were not overwritten.", "Failures: a; b"]);
  });

  it("reads no data-through date when there is no freshness file or no date in it", () => {
    const dir = makeTempDir();
    expect(readDataThroughDate(dataPaths(dir))).toBeNull();
    writeFileSync(join(dir, "data_freshness.json"), '{"refreshed_at": "x"}');
    expect(readDataThroughDate(dataPaths(dir))).toBeNull();
    writeFileSync(join(dir, "data_freshness.json"), '{"data_through_date": "2026-11-25"}');
    expect(readDataThroughDate(dataPaths(dir))).toBe("2026-11-25");
  });
});
