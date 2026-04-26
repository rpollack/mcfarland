import type { DailyMatchupContext, HitterRecord, PitcherRecord, PlayerType } from "./types.js";
import { formatEra, formatPercentage, formatStatValue } from "./utils.js";

function formatLine(label: string, value: string | number | null | undefined): string {
  return `${label}: ${value === null || value === undefined || value === "" ? "N/A" : value}`;
}

function formatWeather(matchup: DailyMatchupContext): string {
  const weather = matchup.weather;
  if (!weather) {
    return "N/A";
  }
  return [weather.condition, weather.temp ? `${weather.temp} degrees` : null, weather.wind].filter(Boolean).join(", ") || "N/A";
}

function formatMatchup(matchup: DailyMatchupContext, type: PlayerType): string[] {
  if (matchup.matchupStatus === "missing_mlbam_id") {
    return ["Matchup status: Missing MLBAM ID; daily MLB context is unavailable."];
  }
  if (matchup.matchupStatus === "no_game") {
    return [
      "Matchup status: No MLB game found for this player today.",
      "Instruction: If there is no game, the recommendation must be SIT.",
    ];
  }

  return [
    formatLine("Game status", matchup.gameStatus),
    formatLine("Postponed", matchup.isPostponed ? "Yes" : "No"),
    formatLine("Player team", matchup.playerTeam?.name),
    formatLine("Opponent", matchup.opponent?.name),
    formatLine("Home/Away", matchup.homeAway),
    formatLine("Game date/time", matchup.gameDate),
    formatLine("Venue", matchup.venue),
    formatLine("Weather", formatWeather(matchup)),
    formatLine("Selected player bats", matchup.selectedPlayerHandedness?.batSide),
    formatLine("Selected player throws", matchup.selectedPlayerHandedness?.pitchHand),
    formatLine("Opposing probable starter", matchup.opposingStarter?.name),
    formatLine("Opposing starter throws", matchup.opposingStarter?.pitchHand),
    formatLine("Selected team probable starter", matchup.selectedTeamStarter?.name),
    formatLine("Selected pitcher is probable starter", type === "pitcher" ? (matchup.isProbableStarter ? "Yes" : "No") : "N/A"),
    formatLine("Platoon matchup", matchup.platoonLabel),
    formatLine("Lineup status", matchup.lineupStatus),
  ];
}

function buildFantasyPlayerLines(player: HitterRecord | PitcherRecord, type: PlayerType): string[] {
  if (type === "hitter") {
    const hitter = player as HitterRecord;
    return [
      `Player: ${hitter.Name} (Hitter)`,
      formatLine("PA", formatStatValue(hitter.PA_cur, 0)),
      formatLine("AVG", formatStatValue(hitter.AVG_cur)),
      formatLine("OBP", formatStatValue(hitter.OBP_cur)),
      formatLine("SLG", formatStatValue(hitter.SLG_cur)),
      formatLine("wOBA", formatStatValue(hitter.wOBA_cur)),
      formatLine("xwOBA", formatStatValue(hitter.xwOBA_cur)),
      formatLine("K%", formatPercentage(hitter.K_pct_cur)),
      formatLine("BB%", formatPercentage(hitter.BB_pct_cur)),
      formatLine("Barrel%", formatPercentage(hitter.Barrel_pct_cur)),
      formatLine("xwOBA last 3 years", formatStatValue(hitter.xwOBA_l3)),
      formatLine("xwOBA diff vs last 3 years", formatStatValue(hitter.xwOBA_diff)),
    ];
  }

  const pitcher = player as PitcherRecord;
  return [
    `Player: ${pitcher.Name} (Pitcher)`,
    formatLine("Role", pitcher.position ?? "Pitcher"),
    formatLine("TBF", formatStatValue(pitcher.tbf, 0)),
    formatLine("ERA", formatEra(pitcher.era_cur)),
    formatLine("xERA", formatEra(pitcher.xera_cur)),
    formatLine("K%", formatPercentage(pitcher.k_percent_cur)),
    formatLine("BB%", formatPercentage(pitcher.bb_percent_cur)),
    formatLine("K-BB%", formatPercentage(pitcher.k_minus_bb_percent_cur)),
    formatLine("CSW%", formatPercentage(pitcher.csw_percent_cur)),
    formatLine("Barrel% allowed", formatPercentage(pitcher.barrel_percent_cur)),
    formatLine("xERA last 3 years", formatEra(pitcher.xera_l3)),
    formatLine("xERA diff vs last 3 years", formatEra(pitcher.xera_diff)),
  ];
}

export function buildFantasyDailyMatchupPrompt(
  player: HitterRecord | PitcherRecord,
  type: PlayerType,
  matchup: DailyMatchupContext
): string {
  return [
    "You are McFARLAND's fantasy baseball daily lineup assistant.",
    "Task: Use the player stats and fresh MLB matchup context below to make one clear daily fantasy recommendation.",
    "",
    "Required output:",
    '- Return raw JSON only with exactly these keys: {"decision":"START|SIT","confidence":"Low|Medium|High","headline":"...","analysis":"..."}',
    "- decision must be exactly START or SIT.",
    "- confidence must be exactly Low, Medium, or High.",
    "- The recommendation must be unambiguous and actionable. Do not hedge with Start/Sit, maybe, or depends as the decision.",
    "- Headline should focus on the main fantasy edge or risk, such as player quality, matchup, role, opponent, venue, or pitcher status.",
    "- Do not mention confirmed lineup status in the headline; lineup has its own UI badge. Use lineup status in analysis only when it changes the recommendation or confidence.",
    "- If there is no game today, the game is postponed, the hitter is confirmed not in the lineup, or a pitcher is not today's probable starter, strongly prefer SIT and say why.",
    "- If key matchup facts are unavailable, still give a START or SIT decision and lower the confidence.",
    "- Keep analysis concise: 2 short paragraphs maximum.",
    "- Do not invent injuries, batting order, lineup status, weather, or opponent details that are not listed below.",
    "",
    "--- Player stats ---",
    ...buildFantasyPlayerLines(player, type),
    "",
    "--- Fresh MLB matchup data ---",
    ...formatMatchup(matchup, type),
  ].join("\n");
}

export function serializeFantasyCachedPayload(payload: {
  decision: "START" | "SIT";
  confidence: "Low" | "Medium" | "High";
  analysis: string;
}): string {
  return JSON.stringify(payload);
}

export function parseFantasyCachedPayload(headline: string, analysis: string): {
  decision: "START" | "SIT";
  confidence: "Low" | "Medium" | "High";
  headline: string;
  analysis: string;
} | null {
  try {
    const parsed = JSON.parse(analysis) as Partial<{
      decision: "START" | "SIT";
      confidence: "Low" | "Medium" | "High";
      analysis: string;
    }>;
    const confidence = parsed.confidence;
    if (
      (parsed.decision !== "START" && parsed.decision !== "SIT") ||
      (confidence !== "Low" && confidence !== "Medium" && confidence !== "High")
    ) {
      return null;
    }
    if (typeof parsed.analysis !== "string") {
      return null;
    }
    return {
      decision: parsed.decision,
      confidence,
      headline,
      analysis: parsed.analysis,
    };
  } catch {
    return null;
  }
}
