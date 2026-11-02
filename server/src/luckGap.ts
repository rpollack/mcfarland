import type { HitterRecord, PitcherRecord, PlayerType } from "./types.js";

/** Minimum plate appearances (hitters) or batters faced (pitchers) for a player to be featured. */
export const MIN_FEATURED_SAMPLE = 300;

export interface LuckStats {
  /** wOBA for hitters, ERA for pitchers. */
  actual: number | null;
  /** xwOBA for hitters, xERA for pitchers. */
  expected: number | null;
  /** Positive when results trail expectations (xwOBA - wOBA, or ERA - xERA); negative when they run ahead. */
  gap: number | null;
}

function finite(value: number | null | undefined): number | null {
  return value !== null && value !== undefined && Number.isFinite(value) ? value : null;
}

export function getLuckStats(type: PlayerType, record: HitterRecord | PitcherRecord): LuckStats {
  if (type === "hitter") {
    const hitter = record as HitterRecord;
    const actual = finite(hitter.wOBA_cur);
    const expected = finite(hitter.xwOBA_cur);
    return { actual, expected, gap: actual !== null && expected !== null ? expected - actual : null };
  }

  const pitcher = record as PitcherRecord;
  const actual = finite(pitcher.era_cur);
  const expected = finite(pitcher.xera_cur);
  return { actual, expected, gap: actual !== null && expected !== null ? actual - expected : null };
}

export function getSampleSize(type: PlayerType, record: HitterRecord | PitcherRecord): number {
  return finite(type === "hitter" ? (record as HitterRecord).PA_cur : (record as PitcherRecord).tbf) ?? 0;
}
