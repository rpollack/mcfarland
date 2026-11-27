// Where the data lives and which season the refresh is building. Run everything from the repository root.

import { join } from "node:path";

// Yearly rollover, in November: build the baseline files for the season that just finished,
//   npm run refresh:baseline -- <that year>
// and in March, once the new season has games (before that the stats APIs return no rows and the refresh stops), bump
// CURRENT_YEAR. The three seasons before CURRENT_YEAR are the baseline, oldest first; the weights make last season count five
// times as much as the oldest one.
export const CURRENT_YEAR = 2027;

/**
 * Where the data lives, relative to the repository root (the working directory for every run). The app's server reads the same
 * folder (DATA_ROOT in server/src/dataStore.ts), so moving it means changing both.
 */
export const DATA_DIR = "data";

export type DataPaths = { dataDir: string; baselineDir: string; scratchDir: string; file: (name: string) => string };

export function dataPaths(dataDir: string = DATA_DIR): DataPaths {
  return {
    dataDir,
    baselineDir: join(dataDir, "baselines"),
    // Candidate files and reports from each run; gitignored, rewritten every time.
    scratchDir: join(dataDir, "scratch"),
    file: (name) => join(dataDir, name),
  };
}
