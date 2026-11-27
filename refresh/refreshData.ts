// The daily data refresh, and the one-off baseline builder. Replaces the original R refresh/refresh_data.R.
//
//   npm run refresh                                  rebuild the published CSVs from the MLB Stats API and Baseball Savant
//   npm run refresh:baseline -- <year> [--force]     write the two baseline files for a finished season
//
// Run from the repository root. Sources of the data are in dataSources.ts; the transformations and the gates on the published CSVs
// are in pipeline.ts. The yearly rollover steps are in config.ts.

import { copyFileSync, existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { pathToFileURL } from "node:url";
import { CURRENT_YEAR, dataPaths, DATA_DIR, type DataPaths } from "./config.ts";
import {
  ANY_SEASON_MIN_ROWS, buildBaselineSeason, buildSeasonStats, defaultHttpGet, type HttpGet, type Log,
} from "./dataSources.ts";
import { dataThroughDate, formatRefreshedAt, readCsvTable, writeCsv, writeJson, type Json } from "./format.ts";
import {
  buildHitterOutput, buildPitcherOutput, buildPlayerLookup, buildServedPlayerInventory, checkOutputSanity, checkPlayerIdCollisions,
  readHitterBaselineSeasons, readPitcherBaselineSeasons, readPlayerIdMap, validateMigrationParity, type ParityReport,
} from "./pipeline.ts";
import {
  baselineSeasonsFor, HITTER_OUTPUT_COLUMNS, PITCHER_OUTPUT_COLUMNS, PLAYER_LOOKUP_COLUMNS, SERVED_INVENTORY_COLUMNS, type Table,
} from "./types.ts";

export type RefreshOptions = {
  paths?: DataPaths;
  currentYear?: number;
  /** The clock; tests pin it. */
  now?: Date;
  /** The network; tests pass fixtures. */
  httpGet?: HttpGet;
  log?: Log;
  waitSeconds?: number;
};

export type RefreshResult = { promoted: boolean; report: ParityReport };

/** The data-through date recorded by the last promoted run, or null when there is none. */
export function readDataThroughDate(paths: DataPaths): string | null {
  const file = paths.file("data_freshness.json");
  if (!existsSync(file)) return null;
  const parsed = JSON.parse(readFileSync(file, "utf8")) as { data_through_date?: unknown };
  return parsed.data_through_date === undefined || parsed.data_through_date === null ? null : String(parsed.data_through_date);
}

function readLocked(paths: DataPaths, name: string): Table {
  return readCsvTable(readFileSync(paths.file(name), "utf8"), ["PlayerId", "mlbamid"]);
}

/** Copies the validated candidate files over production; refuses (and leaves production alone) unless the report passed. */
export function promoteCandidateOutputs(report: ParityReport, paths: DataPaths, log: Log): boolean {
  if (report.status !== "pass") {
    log("Migration parity failed; production CSVs were not overwritten.");
    log(`Failures: ${report.failures.join("; ")}`);
    return false;
  }
  copyFileSync(`${paths.scratchDir}/candidate_full_stats_hitters.csv`, paths.file("full_stats_hitters.csv"));
  copyFileSync(`${paths.scratchDir}/candidate_full_stats_pitchers.csv`, paths.file("full_stats_pitchers.csv"));
  copyFileSync(`${paths.scratchDir}/candidate_player_lookup.csv`, paths.file("player_lookup.csv"));
  return true;
}

export async function refreshData(options: RefreshOptions = {}): Promise<RefreshResult> {
  const {
    paths = dataPaths(), currentYear = CURRENT_YEAR, now = new Date(), httpGet = defaultHttpGet, log = console.log, waitSeconds,
  } = options;
  const baselineSeasons = baselineSeasonsFor(currentYear);

  const idMap = readPlayerIdMap(paths.file("player_id_map.csv"));
  const hitterBaselineHistory = readHitterBaselineSeasons(baselineSeasons, paths.baselineDir);

  // The season in progress: fewer rows than a full season is normal, so only an empty response is refused here; checkOutputSanity
  // below catches a feed that went thin or empty.
  const seasonStats = await buildSeasonStats(currentYear, { minRows: ANY_SEASON_MIN_ROWS, httpGet, log, waitSeconds });

  const fullStatsHitters = buildHitterOutput(seasonStats.hitters, hitterBaselineHistory, idMap);
  const pitcherBaselineHistory = readPitcherBaselineSeasons(baselineSeasons, paths.baselineDir);
  const fullStatsPitchers = buildPitcherOutput(seasonStats.pitchers, pitcherBaselineHistory, currentYear, idMap);
  checkPlayerIdCollisions([...fullStatsHitters.rows, ...fullStatsPitchers.rows], idMap);

  // Validate the candidates, then promote.
  const candidateLookup = buildPlayerLookup(fullStatsHitters, fullStatsPitchers);

  const lockedHitters = readLocked(paths, "full_stats_hitters.csv");
  lockedHitters.rows.forEach((row) => (row["player_type"] = "hitter"));
  const lockedPitchers = readLocked(paths, "full_stats_pitchers.csv");
  lockedPitchers.rows.forEach((row) => (row["player_type"] = "pitcher"));
  const lockedLookup = readLocked(paths, "player_lookup.csv");
  const servedInventory = buildServedPlayerInventory(lockedHitters, lockedPitchers, lockedLookup);

  const lockedDataThroughDate = readDataThroughDate(paths);
  const candidateDataThroughDate = dataThroughDate(now);

  const sanityFailures = checkOutputSanity(fullStatsHitters, fullStatsPitchers, lockedHitters, lockedPitchers, lockedDataThroughDate, currentYear);
  const report = validateMigrationParity(
    lockedHitters, lockedPitchers, lockedLookup, fullStatsHitters, fullStatsPitchers, candidateLookup,
    lockedDataThroughDate, candidateDataThroughDate, sanityFailures,
  );

  mkdirSync(paths.scratchDir, { recursive: true });
  writeFileSync(`${paths.scratchDir}/candidate_full_stats_hitters.csv`, writeCsv(HITTER_OUTPUT_COLUMNS, fullStatsHitters.rows));
  writeFileSync(`${paths.scratchDir}/candidate_full_stats_pitchers.csv`, writeCsv(PITCHER_OUTPUT_COLUMNS, fullStatsPitchers.rows));
  writeFileSync(`${paths.scratchDir}/candidate_player_lookup.csv`, writeCsv(PLAYER_LOOKUP_COLUMNS, candidateLookup.rows));
  writeFileSync(`${paths.scratchDir}/served_player_inventory.csv`, writeCsv(SERVED_INVENTORY_COLUMNS, servedInventory.rows));
  writeFileSync(`${paths.scratchDir}/migration_parity_report.json`, writeJson(report.json) + "\n");

  const promoted = promoteCandidateOutputs(report, paths, log);
  if (promoted) {
    const freshness: Json = {
      refreshed_at: formatRefreshedAt(now),
      refreshed_timezone: "UTC",
      data_through_date: candidateDataThroughDate,
    };
    writeFileSync(paths.file("data_freshness.json"), writeJson(freshness) + "\n");
  }

  const withMlbId = (table: Table) => table.rows.filter((row) => row["mlbamid"] !== null && row["mlbamid"] !== "").length;
  log(`Data refreshed at: ${now.toISOString()}`);
  log(`Migration parity status: ${report.status}`);
  log(`Candidate hitters: ${fullStatsHitters.rows.length}`);
  log(`Candidate pitchers: ${fullStatsPitchers.rows.length}`);
  log(`Candidate total players: ${candidateLookup.rows.length}`);
  log(`Production files promoted: ${promoted ? "TRUE" : "FALSE"}`);
  log(`Hitters with MLB IDs: ${withMlbId(fullStatsHitters)} / ${fullStatsHitters.rows.length}`);
  log(`Pitchers with MLB IDs: ${withMlbId(fullStatsPitchers)} / ${fullStatsPitchers.rows.length}`);
  return { promoted, report };
}

/** The command line. Returns the process exit code: 0 on success, 1 when promotion was refused. */
export async function main(args: string[], env: NodeJS.ProcessEnv = process.env): Promise<number> {
  const paths = dataPaths(env["DATA_DIR"] ?? DATA_DIR);
  // build-baseline-season <year> [--force]: writes baseline-hitters-<year>.csv and baseline-pitchers-<year>.csv from the MLB Stats
  // API and Baseball Savant. Refuses to overwrite existing files without --force.
  if (args[0] === "build-baseline-season") {
    // (The R script fell through to the daily refresh when the year was left off; asking for a baseline and getting a refresh is
    // a surprise, so that is an error here.)
    const year = Number.parseInt(args[1] ?? "", 10);
    if (!Number.isInteger(year)) throw new Error(`build-baseline-season needs a year, got "${args[1] ?? ""}"`);
    await buildBaselineSeason(year, { force: args.includes("--force"), dir: paths.baselineDir });
    return 0;
  }
  const { promoted } = await refreshData({ paths });
  // A refused promotion must fail the run: otherwise the workflow looks green while the published data stands still.
  return promoted ? 0 : 1;
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  main(process.argv.slice(2)).then(
    (code) => process.exit(code),
    (error) => {
      console.error(error instanceof Error ? error.message : error);
      process.exit(1);
    },
  );
}
