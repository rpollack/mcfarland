export type PlayerType = "hitter" | "pitcher";

export interface PlayerSummary {
  id: string;
  name: string;
  type: PlayerType;
  mlbamid?: string | null;
}

export interface HitterRecord {
  Name: string;
  PlayerId: string;
  Age: number | null;
  mlbamid?: string | null;
  AVG_cur: number | null;
  OBP_cur: number | null;
  SLG_cur: number | null;
  K_pct_cur: number | null;
  BB_pct_cur: number | null;
  Barrel_pct_cur: number | null;
  BABIP_cur: number | null;
  wOBA_cur: number | null;
  xwOBA_cur: number | null;
  xwOBA_wOBA_gap_cur: number | null;
  PA_cur: number | null;
  AVG_l3: number | null;
  OBP_l3: number | null;
  SLG_l3: number | null;
  K_pct_l3: number | null;
  BB_pct_l3: number | null;
  Barrel_pct_l3: number | null;
  BABIP_l3: number | null;
  wOBA_l3: number | null;
  xwOBA_l3: number | null;
  xwOBA_wOBA_gap_l3: number | null;
  PA_l3: number | null;
  AVG_diff: number | null;
  OBP_diff: number | null;
  SLG_diff: number | null;
  K_pct_diff: number | null;
  BB_pct_diff: number | null;
  Barrel_pct_diff: number | null;
  BABIP_diff: number | null;
  wOBA_diff: number | null;
  xwOBA_diff: number | null;
  xwOBA_wOBA_gap_diff: number | null;
}

export interface PitcherRecord {
  PlayerId: string;
  mlbamid?: string | null;
  Name: string;
  Age: number | null;
  position?: string | null;
  tbf: number | null;
  era_cur: number | null;
  era_l3: number | null;
  era_diff: number | null;
  xera_cur: number | null;
  xera_l3: number | null;
  xera_diff: number | null;
  babip_cur: number | null;
  babip_l3: number | null;
  babip_diff: number | null;
  barrel_percent_cur: number | null;
  barrel_percent_l3: number | null;
  barrel_percent_diff: number | null;
  k_percent_cur: number | null;
  k_percent_l3: number | null;
  k_percent_diff: number | null;
  bb_percent_cur: number | null;
  bb_percent_l3: number | null;
  bb_percent_diff: number | null;
  k_minus_bb_percent_cur: number | null;
  k_minus_bb_percent_l3: number | null;
  k_minus_bb_percent_diff: number | null;
  csw_percent_cur: number | null;
  csw_percent_l3: number | null;
  csw_percent_diff: number | null;
  o_swing_percent_cur: number | null;
  o_swing_percent_l3: number | null;
  o_swing_percent_diff: number | null;
  lob_percent_cur: number | null;
  lob_percent_l3: number | null;
  lob_percent_diff: number | null;
}

export type PlayerRecord = HitterRecord | PitcherRecord;

export interface PlayerDetail<T extends PlayerRecord> {
  player: T;
  quickInsight: string;
}

export interface ComparisonResponse<T extends PlayerRecord> {
  players: T[];
  recommendedPlayerId: string | null;
}

export interface AnalysisResponse {
  prompt: string;
  persona: string;
  analysis: string;
  cached: boolean;
}

export interface Vibe {
  id: string;
  label: string;
  description: string;
}

export interface AboutContent {
  heading: string;
  paragraphs: string[];
}
