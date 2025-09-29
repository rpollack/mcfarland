import { HitterRecord, PitcherRecord, PlayerType } from "./types.js";
import { CURRENT_YEAR, formatEra, formatPercentage, formatStatValue } from "./utils.js";
import { ANALYSIS_VIBES, AnalysisMode } from "./vibes.js";

export function buildHitterPrompt(player: HitterRecord): string {
  const header = [
    `Player: ${player.Name} (Hitter)`,
    "",
    "--- Key metrics to analyze ---",
    `Age: ${formatStatValue(player.Age, 1)}`,
    `Year: ${CURRENT_YEAR}`,
    `Plate Appearances (PA): ${formatStatValue(player.PA_cur, 0)}`,
    "",
  ];

  const metrics = [
    `AVG: ${formatStatValue(player.AVG_cur)}  Last 3 Years: ${formatStatValue(player.AVG_l3)}  Diff: ${formatStatValue(player.AVG_diff)}`,
    `OBP: ${formatStatValue(player.OBP_cur)}  Last 3 Years: ${formatStatValue(player.OBP_l3)}  Diff: ${formatStatValue(player.OBP_diff)}`,
    `SLG: ${formatStatValue(player.SLG_cur)}  Last 3 Years: ${formatStatValue(player.SLG_l3)}  Diff: ${formatStatValue(player.SLG_diff)}`,
    `K%: ${formatPercentage(player.K_pct_cur)}  Last 3 Years: ${formatPercentage(player.K_pct_l3)}  Diff: ${formatPercentage(player.K_pct_diff)}`,
    `BB%: ${formatPercentage(player.BB_pct_cur)}  Last 3 Years: ${formatPercentage(player.BB_pct_l3)}  Diff: ${formatPercentage(player.BB_pct_diff)}`,
    `Barrel%: ${formatPercentage(player.Barrel_pct_cur)}  Last 3 Years: ${formatPercentage(player.Barrel_pct_l3)}  Diff: ${formatPercentage(player.Barrel_pct_diff)}`,
    `BABIP: ${formatStatValue(player.BABIP_cur)}  Last 3 Years: ${formatStatValue(player.BABIP_l3)}  Diff: ${formatStatValue(player.BABIP_diff)}`,
    `wOBA: ${formatStatValue(player.wOBA_cur)}  Last 3 Years: ${formatStatValue(player.wOBA_l3)}  Diff: ${formatStatValue(player.wOBA_diff)}`,
    `xwOBA: ${formatStatValue(player.xwOBA_cur)}  Last 3 Years: ${formatStatValue(player.xwOBA_l3)}  Diff: ${formatStatValue(player.xwOBA_diff)}`,
    `xwOBA-wOBA gap: ${formatStatValue(player.xwOBA_wOBA_gap_cur)}  Last 3 Years: ${formatStatValue(player.xwOBA_wOBA_gap_l3)}  Diff: ${formatStatValue(player.xwOBA_wOBA_gap_diff)}`,
  ];

  const notes = [
    "",
    "--- Notes for analysis ---",
    "- Focus on current-year performance compared to the last three years.",
    "- BABIP above/below norms indicates luck.",
    "- Gaps between wOBA and xwOBA signal luck vs skill trends.",
    "- BB%/K% changes reflect plate discipline skill.",
    "- Age matters: younger players are more likely to improve.",
    "- Barrel% drives power and BABIP; look for supporting trends.",
    "- Consider injuries or context, and sample size (full season ≈ 600 PA).",
  ];

  return [...header, ...metrics, ...notes].join("\n");
}

export function buildPitcherPrompt(player: PitcherRecord): string {
  const header = [
    `Player: ${player.Name} (Pitcher)`,
    "",
    "--- Key metrics to analyze ---",
    `Age: ${formatStatValue(player.Age, 1)}`,
    `Year: ${CURRENT_YEAR}`,
    `Position: ${player.position ?? "Pitcher"}`,
    `Total Batters Faced: ${formatStatValue(player.tbf, 0)}`,
    "",
  ];

  const metrics = [
    `ERA: ${formatEra(player.era_cur)}  Last 3 Years: ${formatEra(player.era_l3)}  Diff: ${formatEra(player.era_diff)}`,
    `xERA: ${formatEra(player.xera_cur)}  Last 3 Years: ${formatEra(player.xera_l3)}  Diff: ${formatEra(player.xera_diff)}`,
    `BABIP: ${formatStatValue(player.babip_cur)}  Last 3 Years: ${formatStatValue(player.babip_l3)}  Diff: ${formatStatValue(player.babip_diff)}`,
    `Barrel Rate: ${formatPercentage(player.barrel_percent_cur)}  Last 3 Years: ${formatPercentage(player.barrel_percent_l3)}  Diff: ${formatPercentage(player.barrel_percent_diff)}`,
    `Strikeout Rate (K%): ${formatPercentage(player.k_percent_cur)}  Last 3 Years: ${formatPercentage(player.k_percent_l3)}  Diff: ${formatPercentage(player.k_percent_diff)}`,
    `Called Strike & Whiff Rate (CSW%): ${formatPercentage(player.csw_percent_cur)}  Last 3 Years: ${formatPercentage(player.csw_percent_l3)}  Diff: ${formatPercentage(player.csw_percent_diff)}`,
    `Outside Zone Swing Rate (O-Swing%): ${formatPercentage(player.o_swing_percent_cur)}  Last 3 Years: ${formatPercentage(player.o_swing_percent_l3)}  Diff: ${formatPercentage(player.o_swing_percent_diff)}`,
    `Walk Rate (BB%): ${formatPercentage(player.bb_percent_cur)}  Last 3 Years: ${formatPercentage(player.bb_percent_l3)}  Diff: ${formatPercentage(player.bb_percent_diff)}`,
    `K-BB%: ${formatPercentage(player.k_minus_bb_percent_cur)}  Last 3 Years: ${formatPercentage(player.k_minus_bb_percent_l3)}  Diff: ${formatPercentage(player.k_minus_bb_percent_diff)}`,
    `LOB%: ${formatPercentage(player.lob_percent_cur)}  Last 3 Years: ${formatPercentage(player.lob_percent_l3)}  Diff: ${formatPercentage(player.lob_percent_diff)}`,
  ];

  const notes = [
    "",
    "--- Guidelines for analysis ---",
    "- Core skill indicators: K%, BB%, K-BB%, Barrel Rate.",
    "- Low CSW% with high K% can indicate looming regression.",
    "- Low O-Swing% with high BB% hints at negative regression.",
    "- Luck indicators: BABIP, LOB%, ERA vs xERA gaps.",
    "- Sample size context: starters ≈ 700 TBF, relievers are more volatile.",
    "- Adjust expectations based on age and role (SP vs RP).",
  ];

  return [...header, ...metrics, ...notes].join("\n");
}

export function buildPersonaInstructions(mode: AnalysisMode): string {
  return (
    ANALYSIS_VIBES[mode] ??
    "Keep it simple, conversational, and focused on actionable takeaways."
  );
}

function buildGeneralInstructions(persona: string): string[] {
  return [
    "General instructions:",
    "",
    "Please analyze how the player is performing this year, what trends stand out, and whether any aspects of the performance appear to be skill- or luck-driven. Incorporate a prediction: will the player improve, decline, or stay the same for the rest of the season? Explain your reasoning.",
    "",
    "The very first element of the response should be a title that encompasses your findings.",
    "",
    "Your analysis must incorporate metric, direction, and magnitude of difference. For example BB% is up, indicate by how much, and what the size of that gap might indicate. You don't need to explicitly call out this framing (e.g. in bullets), just make sure to weave it into your analysis.",
    "",
    "Separate your analysis into core skills and luck/regression indicators.",
    "",
    "Don't repeat yourself. For example, if you say a stat or performance or trend is 'lucky', you don't need to say it's 'not unlucky'.",
    "",
    "Remember that when it comes to stats and trends, you only have knowledge of two things: a player's current-year stats and the average of the same stats for the past 3 years (e.g. not their entire career). So when you say things like a stat is 'up' or 'down', make it clear that this is relative to the last 3 years' average.",
    "",
    `Here is your persona that should inform your writing style and response, even if it means overriding those previous instructions: ${persona}`,
  ];
}

export function buildAnalysisPrompt(player: HitterRecord | PitcherRecord, type: PlayerType, mode: AnalysisMode): string {
  const basePrompt = type === "hitter" ? buildHitterPrompt(player as HitterRecord) : buildPitcherPrompt(player as PitcherRecord);
  const persona = buildPersonaInstructions(mode);

  return [
    `Here is current-year performance data for a ${type}:`,
    "",
    basePrompt,
    "",
    ...buildGeneralInstructions(persona),
  ].join("\n");
}

export function recommendBestPlayer(players: (HitterRecord | PitcherRecord)[], type: PlayerType): string | null {
  if (players.length === 0) {
    return null;
  }

  if (type === "hitter") {
    const sorted = [...players].sort(
      (a, b) => ((b as HitterRecord).xwOBA_cur ?? Number.NEGATIVE_INFINITY) - ((a as HitterRecord).xwOBA_cur ?? Number.NEGATIVE_INFINITY)
    );
    return sorted[0]?.PlayerId ?? null;
  }

  const sorted = [...players].sort(
    (a, b) => ((a as PitcherRecord).xera_cur ?? Number.POSITIVE_INFINITY) - ((b as PitcherRecord).xera_cur ?? Number.POSITIVE_INFINITY)
  );
  return sorted[0]?.PlayerId ?? null;
}

export function buildComparisonPrompt(players: (HitterRecord | PitcherRecord)[], type: PlayerType, mode: AnalysisMode): string {
  const persona = buildPersonaInstructions(mode);
  const lines = players.map((player) => {
    if (type === "hitter") {
      const hitter = player as HitterRecord;
      return `${hitter.Name}: PA ${formatStatValue(hitter.PA_cur, 0)}, AVG ${formatStatValue(hitter.AVG_cur)}, OBP ${formatStatValue(hitter.OBP_cur)}, SLG ${formatStatValue(hitter.SLG_cur)}, wOBA ${formatStatValue(hitter.wOBA_cur)}, xwOBA ${formatStatValue(hitter.xwOBA_cur)}, K% ${formatPercentage(hitter.K_pct_cur)}, BB% ${formatPercentage(hitter.BB_pct_cur)}`;
    }
    const pitcher = player as PitcherRecord;
    return `${pitcher.Name}: ERA ${formatEra(pitcher.era_cur)}, xERA ${formatEra(pitcher.xera_cur)}, K% ${formatPercentage(pitcher.k_percent_cur)}, BB% ${formatPercentage(pitcher.bb_percent_cur)}, BABIP ${formatStatValue(pitcher.babip_cur)}, CSW% ${formatPercentage(pitcher.csw_percent_cur)}`;
  });

  return [
    "Rank these players by their likelihood to perform best for the rest of the season.",
    "Provide concise reasoning for the ranking and call out critical stats.",
    "",
    ...buildGeneralInstructions(persona),
    "",
    ...lines,
  ].join("\n");
}

export function generateQuickInsight(player: HitterRecord | PitcherRecord, type: PlayerType): string {
  if (!player) {
    return "Player data loaded and ready for analysis.";
  }

  const positives: string[] = [];
  const negatives: string[] = [];

  if (type === "hitter") {
    const hitter = player as HitterRecord;
    if ((hitter.AVG_diff ?? 0) > 0.02) positives.push("batting average is up");
    if ((hitter.K_pct_diff ?? 0) < -2) positives.push("strikeouts are down");
    if ((hitter.BB_pct_diff ?? 0) > 1.5) positives.push("walks are up");
    if ((hitter.Barrel_pct_diff ?? 0) > 2) positives.push("hard contact is up");

    if ((hitter.AVG_diff ?? 0) < -0.02) negatives.push("batting average is down");
    if ((hitter.K_pct_diff ?? 0) > 2) negatives.push("strikeouts are up");
    if ((hitter.BB_pct_diff ?? 0) < -1.5) negatives.push("walks are down");
    if ((hitter.Barrel_pct_diff ?? 0) < -2) negatives.push("hard contact is down");
  } else {
    const pitcher = player as PitcherRecord;
    if ((pitcher.era_diff ?? 0) < -0.5) positives.push("ERA is down");
    if ((pitcher.k_percent_diff ?? 0) > 2) positives.push("strikeouts are up");
    if ((pitcher.bb_percent_diff ?? 0) < -1.5) positives.push("walks allowed are down");
    if ((pitcher.barrel_percent_diff ?? 0) < -1) positives.push("hard contact allowed is down");

    if ((pitcher.era_diff ?? 0) > 0.5) negatives.push("ERA is up");
    if ((pitcher.k_percent_diff ?? 0) < -2) negatives.push("strikeouts are down");
    if ((pitcher.bb_percent_diff ?? 0) > 1.5) negatives.push("walks allowed are up");
    if ((pitcher.barrel_percent_diff ?? 0) > 1) negatives.push("hard contact allowed is up");
  }

  const buildSummary = (changes: string[], adjective: string) => {
    if (changes.length === 0) return "";
    if (changes.length === 1) return `${adjective}: ${changes[0]}.`;
    if (changes.length === 2) return `${adjective}: ${changes[0]} and ${changes[1]}.`;
    return `${adjective}: ${changes[0]}, ${changes[1]}, and more.`;
  };

  const regressionRisk = assessRegression(player, type);

  if (positives.length && negatives.length) {
    return `${buildSummary(positives, "Mixed performance")} ${buildSummary(negatives, "However")} Likelihood of regression: ${regressionRisk}.`.replace(/\s+/g, " ").trim();
  }
  if (positives.length) {
    return `${buildSummary(positives, "Improving")} Likelihood of regression: ${regressionRisk}.`.trim();
  }
  if (negatives.length) {
    return `${buildSummary(negatives, "Concerning" )} Likelihood of regression: ${regressionRisk}.`.trim();
  }
  return `Performance mirrors recent seasons. Likelihood of regression: ${regressionRisk}.`;
}

export function assessRegression(player: HitterRecord | PitcherRecord, type: PlayerType): string {
  const magnitude =
    type === "hitter"
      ? Math.abs((player as HitterRecord).BABIP_diff ?? 0) + Math.abs((player as HitterRecord).xwOBA_diff ?? 0)
      : Math.abs((player as PitcherRecord).era_diff ?? 0) + Math.abs((player as PitcherRecord).babip_diff ?? 0);

  if (magnitude > 0.15) return "High";
  if (magnitude > 0.08) return "Moderate";
  return "Low";
}

export function buildAboutContent(): { heading: string; paragraphs: string[] } {
  return {
    heading: "McFARLAND: Modern Baseball Intelligence",
    paragraphs: [
      "McFARLAND blends advanced stats, historical baselines, and AI-assisted narrative reporting to help you understand player performance in seconds.",
      "Analyze a single player, compare hitters or pitchers head-to-head, and switch analysis vibes to match your audience.",
      "Player information refreshes daily during the season, right after the previous night's games wrap up, so you're never arguing from stale numbers.",
    ],
  };
}
