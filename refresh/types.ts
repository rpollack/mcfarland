// Column contracts for the refresh: the season tables, the baseline summaries and the published files.
//
// The published CSVs are read by name by the app's server (server/src/dataStore.ts), so a rename or a drop here is a change to
// the app too. Rows are plain objects (see format.ts); these lists give each table its columns and their order.

import type { Row } from "./format.ts";

export type { Row };
export type Table = { columns: string[]; rows: Row[] };

/** The season table written to baseline-hitters-<year>.csv, in column order. */
export const HITTER_SEASON_COLUMNS = [
  "mlbamid", "Name", "Age", "G", "AB", "PA", "H", "2B", "3B", "HR", "BB", "HBP", "SF", "SO", "TB", "Barrels", "wOBA", "xwOBA",
] as const;

/** The season table written to baseline-pitchers-<year>.csv, in column order. */
export const PITCHER_SEASON_COLUMNS = [
  "mlbamid", "name", "age", "position", "g", "gs", "ip", "tbf", "h", "r", "er", "hr", "bb", "ibb", "hbp", "so", "ab", "sf", "x_era",
  "fip", "barrel_percent", "o_swing_percent", "whiff_percent", "babip", "lob_percent",
] as const;

// Every baseline file must carry the columns the summaries use, so a file in another shape fails by name instead of binding in as
// all-NA rows and quietly dropping that season.
export const HITTER_BASELINE_COLUMNS = [
  "mlbamid", "Name", "AB", "PA", "H", "2B", "3B", "HR", "BB", "HBP", "SF", "SO", "TB", "Barrels", "wOBA", "xwOBA",
];
export const PITCHER_BASELINE_COLUMNS = [
  "mlbamid", "name", "position", "er", "ip", "so", "bb", "tbf", "x_era", "fip", "barrel_percent", "o_swing_percent", "babip", "lob_percent",
  "whiff_percent",
];

/** The metrics compared against each player's own baseline. Each gets <metric>_diff and <metric>_lg_adj_diff. */
export const HITTER_METRIC_COLS = ["AVG", "OBP", "SLG", "K_pct", "BB_pct", "Barrel_pct", "BABIP", "wOBA", "xwOBA", "xwOBA_wOBA_gap"];
export const PITCHER_METRIC_COLS = [
  "era", "k_percent", "bb_percent", "k_minus_bb_percent", "xera", "o_swing_percent", "whiff_percent", "barrel_percent", "lob_percent", "babip",
];

export const HITTER_OUTPUT_COLUMNS = [
  "Name", "PlayerId", "AVG_cur", "OBP_cur", "SLG_cur", "K_pct_cur", "BB_pct_cur", "Barrel_pct_cur", "BABIP_cur",
  "wOBA_cur", "xwOBA_cur", "xwOBA_wOBA_gap_cur", "PA_cur", "Age", "mlbamid", "AVG_l3", "OBP_l3", "SLG_l3",
  "K_pct_l3", "BB_pct_l3", "Barrel_pct_l3", "BABIP_l3", "wOBA_l3", "xwOBA_l3", "xwOBA_wOBA_gap_l3", "PA_l3",
  "AVG_diff", "OBP_diff", "SLG_diff", "K_pct_diff", "BB_pct_diff", "Barrel_pct_diff", "BABIP_diff", "wOBA_diff",
  "xwOBA_diff", "xwOBA_wOBA_gap_diff", "player_type", "AVG_lg_adj_diff", "OBP_lg_adj_diff", "SLG_lg_adj_diff",
  "K_pct_lg_adj_diff", "BB_pct_lg_adj_diff", "Barrel_pct_lg_adj_diff", "BABIP_lg_adj_diff", "wOBA_lg_adj_diff",
  "xwOBA_lg_adj_diff", "xwOBA_wOBA_gap_lg_adj_diff",
];

export const PITCHER_OUTPUT_COLUMNS = [
  "PlayerId", "mlbamid", "Name", "season", "Age", "g", "gs", "ip", "tbf", "h", "er", "hr", "so", "bb", "ibb", "hbp",
  "babip_cur", "lob_percent_cur", "fip_cur", "o_swing_percent_cur", "whiff_percent_cur", "x_era", "position",
  "barrel_percent_cur", "xera_cur", "k_percent_cur", "bb_percent_cur", "k_minus_bb_percent_cur", "era_cur",
  "xera_gap", "fip_era_gap", "era_l3", "k_percent_l3", "bb_percent_l3", "k_minus_bb_percent_l3", "xera_l3",
  "era_xera_gap", "fip_l3", "era_fip_gap", "barrel_percent_l3", "o_swing_percent_l3", "babip_l3", "lob_percent_l3",
  "whiff_percent_l3", "era_diff", "k_percent_diff", "bb_percent_diff", "k_minus_bb_percent_diff", "xera_diff",
  "o_swing_percent_diff", "whiff_percent_diff", "barrel_percent_diff", "lob_percent_diff", "babip_diff",
  "player_type", "era_lg_adj_diff", "k_percent_lg_adj_diff", "bb_percent_lg_adj_diff",
  "k_minus_bb_percent_lg_adj_diff", "xera_lg_adj_diff", "o_swing_percent_lg_adj_diff", "whiff_percent_lg_adj_diff",
  "barrel_percent_lg_adj_diff", "lob_percent_lg_adj_diff", "babip_lg_adj_diff",
];

/** player_lookup.csv, in column order (hitters have no `position`, so it is NA for them). */
export const PLAYER_LOOKUP_COLUMNS = [
  "Name", "PlayerId", "Age", "player_type", "mlbamid", "compound_id", "display_name", "position_detail", "position",
];

export const SERVED_INVENTORY_COLUMNS = [
  "PlayerId", "player_type", "display_name", "compound_id", "position_detail", "Name", "Age", "mlbamid", "locked_source_file", "locked_row_identity",
];

/** Output columns that hold text; every other published column is numeric. */
export const TEXT_OUTPUT_COLUMNS: ReadonlySet<string> = new Set(["Name", "PlayerId", "mlbamid", "player_type", "position"]);

/** The three seasons before the current one, oldest first, weighted 1, 3, 5. */
export function baselineSeasonsFor(currentYear: number): { year: number; weight: number }[] {
  return [
    { year: currentYear - 3, weight: 1 },
    { year: currentYear - 2, weight: 3 },
    { year: currentYear - 1, weight: 5 },
  ];
}
