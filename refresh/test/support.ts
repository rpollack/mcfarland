import { cpSync, mkdtempSync, readdirSync, readFileSync, statSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, relative } from "node:path";
import type { HttpGet } from "../dataSources.ts";

export const TEST_DIR = new URL(".", import.meta.url).pathname;
export const fixturePath = (set: string, name: string) => join(TEST_DIR, set, name);

/** A fixture file's exact text, byte-order mark included (Savant's CSVs start with one). */
export function fixtureText(name: string, set = "fixtures"): string {
  return readFileSync(fixturePath(set, name), "utf8");
}

export function fixtureNameForUrl(url: string): string {
  if (url.includes("stats=sabermetrics&group=hitting")) return "mlb_sabermetrics_hitting.json";
  if (url.includes("stats=season&group=hitting")) return "mlb_hitting_season.json";
  if (url.includes("stats=season&group=pitching")) return "mlb_pitching_season.json";
  if (url.includes("expected_statistics?type=batter")) return "savant_expected_batter.csv";
  if (url.includes("statcast?type=batter")) return "savant_statcast_batter.csv";
  if (url.includes("expected_statistics?type=pitcher")) return "savant_expected_pitcher.csv";
  if (url.includes("/custom?")) return "savant_custom_pitcher.csv";
  throw new Error(`no fixture for ${url}`);
}

/**
 * A stand-in for the network that answers each source's URL with a fixture file. Records the URLs asked for. With blankWhiff the
 * Savant custom leaderboard comes back with no whiff% values, the way a reshaped feed would.
 */
export function fixtureHttp(set = "fixtures", requested: string[] = [], blankWhiff = false): HttpGet {
  return async (url) => {
    requested.push(url);
    let name = fixtureNameForUrl(url);
    if (blankWhiff && name === "savant_custom_pitcher.csv") name = "savant_custom_pitcher_blank_whiff.csv";
    return fixtureText(name, set);
  };
}

export function makeTempDir(prefix = "refresh-test-"): string {
  return mkdtempSync(join(tmpdir(), prefix));
}

/** Copies a golden scenario's inputs into a scratch directory laid out like the repository (data/...). */
export function stageInputs(scenario: string): string {
  const dir = makeTempDir(`refresh-${scenario}-`);
  cpSync(join(TEST_DIR, "golden", scenario, "inputs", "data"), join(dir, "data"), { recursive: true });
  return dir;
}

export function listFiles(root: string): string[] {
  const found: string[] = [];
  const walk = (dir: string) => {
    for (const entry of readdirSync(dir)) {
      const path = join(dir, entry);
      if (statSync(path).isDirectory()) walk(path);
      else found.push(relative(root, path));
    }
  };
  walk(root);
  return found.sort();
}
