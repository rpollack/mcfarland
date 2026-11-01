import { getDataFreshness, listPlayers } from "./dataStore.js";
import { getLuckStats, getSampleSize, MIN_FEATURED_SAMPLE } from "./luckGap.js";
import type { PlayerType } from "./types.js";
import { formatEra } from "./utils.js";

const BOARD_SIZE = 5;

export interface MirageRow {
  id: string;
  name: string;
  mlbamid: string | null;
  /** wOBA for hitters, ERA for pitchers. */
  actual: number;
  /** xwOBA for hitters, xERA for pitchers. */
  expected: number;
  /** Positive when results trail expectations; negative when they ran ahead of them. */
  gap: number;
}

export interface MirageBoard {
  season: number;
  playerType: PlayerType;
  /** Results ran ahead of expectations: the most likely to slide. */
  lucky: MirageRow[];
  /** Results ran behind expectations: the most likely to bounce back. */
  unlucky: MirageRow[];
}

export function buildMirageBoard(playerType: PlayerType): MirageBoard {
  const rows: MirageRow[] = [];
  for (const record of listPlayers(playerType)) {
    const { actual, expected, gap } = getLuckStats(playerType, record);
    if (actual === null || expected === null || gap === null || getSampleSize(playerType, record) < MIN_FEATURED_SAMPLE) {
      continue;
    }
    rows.push({
      id: String(record.PlayerId),
      name: record.Name,
      mlbamid: record.mlbamid ?? null,
      actual,
      expected,
      gap,
    });
  }

  return {
    season: Number(getDataFreshness().dataThroughDate.slice(0, 4)),
    playerType,
    lucky: rows
      .filter((row) => row.gap < 0)
      .sort((a, b) => a.gap - b.gap)
      .slice(0, BOARD_SIZE),
    unlucky: rows
      .filter((row) => row.gap > 0)
      .sort((a, b) => b.gap - a.gap)
      .slice(0, BOARD_SIZE),
  };
}

function formatRate(value: number): string {
  return value.toFixed(3).replace(/^0/, "");
}

function describeRow(row: MirageRow, playerType: PlayerType): string {
  return playerType === "hitter"
    ? `${row.name} (wOBA ${formatRate(row.actual)}, xwOBA ${formatRate(row.expected)})`
    : `${row.name} (ERA ${formatEra(row.actual)}, xERA ${formatEra(row.expected)})`;
}

export function buildMirageBoardPrompt(board: MirageBoard): string {
  const hitters = board.playerType === "hitter";
  const group = hitters ? "hitters" : "pitchers";

  return [
    `Write a short introduction to the ${board.season} "mirage board": the ${group} whose ${board.season} results ran furthest ahead of, or behind, what their underlying skills say they deserved.`,
    "",
    hitters
      ? "How to read it: wOBA is what the hitter actually produced. xwOBA is what his contact quality (exit velocity and launch angle) says he should have produced. A hitter whose wOBA is well above his xwOBA ran lucky; one whose wOBA is well below ran unlucky."
      : "How to read it: ERA is what the pitcher actually allowed. xERA is what the quality of contact and strikeouts and walks say he should have allowed. A pitcher whose ERA is well below his xERA ran lucky; one whose ERA is well above ran unlucky.",
    "",
    `Ran lucky (results better than the skills): ${board.lucky.map((row) => describeRow(row, board.playerType)).join("; ")}`,
    `Ran unlucky (results worse than the skills): ${board.unlucky.map((row) => describeRow(row, board.playerType)).join("; ")}`,
    "",
    "Instructions:",
    "- Write exactly 3 short paragraphs of one or two sentences each: what a mirage is, who ran lucky, who ran unlucky.",
    "- Mention at most three players per group, and only players listed above.",
    `- Describe what happened in ${board.season}. Do not predict next season, invent injuries or other context, or give fantasy advice.`,
    "- This is a single season, so say so where it matters rather than overstating.",
  ].join("\n");
}
