export type PlayerType = 'hitter' | 'pitcher';

export interface PlayerSample {
  label: string;
  value: number | null;
}

export interface PlayerSummary {
  id: string;
  playerId: string;
  name: string;
  displayName: string;
  type: PlayerType;
  age: number | null;
  mlbamId?: number | null;
  position?: string | null;
  positionDetail?: string | null;
  sample: PlayerSample;
  photoUrl?: string | null;
}

export interface StatMetricDisplay {
  current: string;
  previous?: string | null;
  diff?: string | null;
}

export interface StatMetric {
  label: string;
  scale: string;
  current: number | null;
  previous: number | null;
  diff: number | null;
  display?: StatMetricDisplay;
}

export interface CacheMetadata {
  refreshed?: string | null;
}

export interface PlayersResponse {
  players: PlayerSummary[];
  type: string;
  count: number;
  updatedAt: string;
  cache?: CacheMetadata;
}

export interface StatlineResponse {
  player: PlayerSummary;
  metrics: StatMetric[];
  updatedAt: string;
  cache?: CacheMetadata;
}

export interface AnalysisResponse {
  player: PlayerSummary;
  mode: string;
  html: string;
  updatedAt: string;
  cache?: CacheMetadata;
}

export interface ComparePlayer {
  player: PlayerSummary;
  metrics: StatMetric[];
}

export interface CompareRecommendation {
  id: string;
  playerId: string;
  name: string;
  displayName: string;
  type: PlayerType;
}

export interface CompareResponse {
  playerType: PlayerType;
  mode: string;
  players: ComparePlayer[];
  analysisHtml: string;
  recommended?: CompareRecommendation | null;
  updatedAt: string;
  cache?: CacheMetadata;
}
