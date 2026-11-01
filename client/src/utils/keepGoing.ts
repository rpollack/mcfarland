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
