import type { PlayerType } from "../types";

const MINUS = "−";

/** Formats a luck gap like ".027" or "0.31", with a sign (+ when results trail expectations). */
export function formatLuckGap(gap: number, type: PlayerType): string {
  const sign = gap > 0 ? "+" : gap < 0 ? MINUS : "";
  const digits = type === "hitter" ? 3 : 2;
  const magnitude = Math.abs(gap).toFixed(digits);
  return `${sign}${type === "hitter" ? magnitude.replace(/^0/, "") : magnitude}`;
}

export function buildCompareHref(type: PlayerType, playerIds: string[]): string {
  const params = new URLSearchParams({ mode: "compare", playerType: type, playerIds: playerIds.join(",") });
  return `/?${params.toString()}`;
}

/** Formats an actual or expected stat: wOBA-style rates like ".349", ERA-style values like "2.12". */
export function formatStatValue(value: number, type: PlayerType): string {
  return type === "hitter" ? value.toFixed(3).replace(/^0/, "") : value.toFixed(2);
}

/** Bar length as a share of the largest gap on the board, never below a visible sliver. */
export function gapBarPercent(gap: number, maxAbsGap: number): number {
  if (maxAbsGap <= 0) {
    return 0;
  }
  return Math.max(4, Math.min(100, (Math.abs(gap) / maxAbsGap) * 100));
}
