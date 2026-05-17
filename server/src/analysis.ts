import { HitterRecord, PitcherRecord, PlayerType } from "./types.js";
import { CURRENT_YEAR, formatEra, formatPercentage, formatStatValue } from "./utils.js";
import { ANALYSIS_VIBES, AnalysisMode } from "./vibes.js";

function formatMetricLine(label: string, current: string, weightedBaseline: string, diff: string, leagueAdjustedDiff: string): string {
  return `${label}: ${current} | WB ${weightedBaseline} | diff ${diff} | diff from league-wide weighted baseline ${leagueAdjustedDiff}`;
}

export function buildHitterPrompt(player: HitterRecord): string {
  const header = [
    `Player: ${player.Name} (Hitter)`,
    "",
    "--- Key metrics to analyze ---",
    "Format: current | WB (weighted baseline) | diff | diff from league-wide weighted baseline (player diff minus league-wide diff)",
    `Age: ${formatStatValue(player.Age, 0)}`,
    `Year: ${CURRENT_YEAR}`,
    `Plate Appearances (PA): ${formatStatValue(player.PA_cur, 0)}`,
    "",
  ];

  const metrics = [
    formatMetricLine("AVG", formatStatValue(player.AVG_cur), formatStatValue(player.AVG_l3), formatStatValue(player.AVG_diff), formatStatValue(player.AVG_lg_adj_diff)),
    formatMetricLine("OBP", formatStatValue(player.OBP_cur), formatStatValue(player.OBP_l3), formatStatValue(player.OBP_diff), formatStatValue(player.OBP_lg_adj_diff)),
    formatMetricLine("SLG", formatStatValue(player.SLG_cur), formatStatValue(player.SLG_l3), formatStatValue(player.SLG_diff), formatStatValue(player.SLG_lg_adj_diff)),
    formatMetricLine("K%", formatPercentage(player.K_pct_cur), formatPercentage(player.K_pct_l3), formatPercentage(player.K_pct_diff), formatPercentage(player.K_pct_lg_adj_diff)),
    formatMetricLine("BB%", formatPercentage(player.BB_pct_cur), formatPercentage(player.BB_pct_l3), formatPercentage(player.BB_pct_diff), formatPercentage(player.BB_pct_lg_adj_diff)),
    formatMetricLine("Barrel%", formatPercentage(player.Barrel_pct_cur), formatPercentage(player.Barrel_pct_l3), formatPercentage(player.Barrel_pct_diff), formatPercentage(player.Barrel_pct_lg_adj_diff)),
    formatMetricLine("BABIP", formatStatValue(player.BABIP_cur), formatStatValue(player.BABIP_l3), formatStatValue(player.BABIP_diff), formatStatValue(player.BABIP_lg_adj_diff)),
    formatMetricLine("wOBA", formatStatValue(player.wOBA_cur), formatStatValue(player.wOBA_l3), formatStatValue(player.wOBA_diff), formatStatValue(player.wOBA_lg_adj_diff)),
    formatMetricLine("xwOBA", formatStatValue(player.xwOBA_cur), formatStatValue(player.xwOBA_l3), formatStatValue(player.xwOBA_diff), formatStatValue(player.xwOBA_lg_adj_diff)),
    formatMetricLine("xwOBA-wOBA gap", formatStatValue(player.xwOBA_wOBA_gap_cur), formatStatValue(player.xwOBA_wOBA_gap_l3), formatStatValue(player.xwOBA_wOBA_gap_diff), formatStatValue(player.xwOBA_wOBA_gap_lg_adj_diff)),
  ];

  const notes = [
    "",
    "--- Notes for analysis ---",
    "- BABIP = luck; wOBA/xwOBA gap = luck vs skill; BB%/K% = discipline; Barrel% = power/BABIP support.",
    "- Age matters; younger players are likelier to improve. Full-season hitter sample is about 600 PA.",
  ];

  return [...header, ...metrics, ...notes].join("\n");
}

export function buildPitcherPrompt(player: PitcherRecord): string {
  const header = [
    `Player: ${player.Name} (Pitcher)`,
    "",
    "--- Key metrics to analyze ---",
    "Format: current | WB (weighted baseline) | diff | diff from league-wide weighted baseline (player diff minus league-wide diff)",
    `Age: ${formatStatValue(player.Age, 0)}`,
    `Year: ${CURRENT_YEAR}`,
    `Position: ${player.position ?? "Pitcher"}`,
    `Total Batters Faced: ${formatStatValue(player.tbf, 0)}`,
    "",
  ];

  const metrics = [
    formatMetricLine("ERA", formatEra(player.era_cur), formatEra(player.era_l3), formatEra(player.era_diff), formatEra(player.era_lg_adj_diff)),
    formatMetricLine("xERA", formatEra(player.xera_cur), formatEra(player.xera_l3), formatEra(player.xera_diff), formatEra(player.xera_lg_adj_diff)),
    formatMetricLine("BABIP", formatStatValue(player.babip_cur), formatStatValue(player.babip_l3), formatStatValue(player.babip_diff), formatStatValue(player.babip_lg_adj_diff)),
    formatMetricLine("Barrel Rate", formatPercentage(player.barrel_percent_cur), formatPercentage(player.barrel_percent_l3), formatPercentage(player.barrel_percent_diff), formatPercentage(player.barrel_percent_lg_adj_diff)),
    formatMetricLine("Strikeout Rate (K%)", formatPercentage(player.k_percent_cur), formatPercentage(player.k_percent_l3), formatPercentage(player.k_percent_diff), formatPercentage(player.k_percent_lg_adj_diff)),
    formatMetricLine("Called Strike & Whiff Rate (CSW%)", formatPercentage(player.csw_percent_cur), formatPercentage(player.csw_percent_l3), formatPercentage(player.csw_percent_diff), formatPercentage(player.csw_percent_lg_adj_diff)),
    formatMetricLine("Outside Zone Swing Rate (O-Swing%)", formatPercentage(player.o_swing_percent_cur), formatPercentage(player.o_swing_percent_l3), formatPercentage(player.o_swing_percent_diff), formatPercentage(player.o_swing_percent_lg_adj_diff)),
    formatMetricLine("Walk Rate (BB%)", formatPercentage(player.bb_percent_cur), formatPercentage(player.bb_percent_l3), formatPercentage(player.bb_percent_diff), formatPercentage(player.bb_percent_lg_adj_diff)),
    formatMetricLine("K-BB%", formatPercentage(player.k_minus_bb_percent_cur), formatPercentage(player.k_minus_bb_percent_l3), formatPercentage(player.k_minus_bb_percent_diff), formatPercentage(player.k_minus_bb_percent_lg_adj_diff)),
    formatMetricLine("LOB%", formatPercentage(player.lob_percent_cur), formatPercentage(player.lob_percent_l3), formatPercentage(player.lob_percent_diff), formatPercentage(player.lob_percent_lg_adj_diff)),
  ];

  const notes = [
    "",
    "--- Guidelines for analysis ---",
    "- Core skills: K%, BB%, K-BB%, Barrel Rate; luck/regression: BABIP, LOB%, ERA/xERA, CSW%/K%, O-Swing%/BB%.",
    "- Sample context: starters about 700 TBF; relievers are more volatile. Adjust for age and SP/RP role.",
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
    "Instructions:",
    "Analyze current season vs weighted baseline only: main trends, skill vs luck, and rest-of-season call (improve/decline/same + confidence).",
    "Use an inverted-pyramid structure: most important conclusion first, then strongest evidence, then caveats/context/ROS judgment.",
    "Use 3-5 short paragraphs separated by blank lines; do not return one solid block of text.",
    "Write complete, natural sentences with varied openings; avoid metric-first fragments and stock openers like 'the clearest read' or 'the biggest takeaway'. Explain baseball meaning before numbers.",
    "Choose the 2-4 most diagnostic signals for this specific player; do not default to the same metrics every time or mention a stat just because it is available.",
    "For each selected metric, convey the takeaway first, then use only the numbers needed to support it.",
    "Do not cram current value, weighted baseline, raw diff, and league-adjusted diff into one sentence or parenthetical. Usually cite one anchor number per signal, sometimes two; avoid stat stacks.",
    "Use the league-adjusted diff to phrase the conclusion, such as 'improved even after accounting for the league' or 'mostly mirrors the league trend.' Mention raw diff only when it changes the interpretation.",
    "Use league-adjusted diff as the main read on whether the player changed relative to league-wide trends; raw diff is supporting context.",
    "When raw diff and league-adjusted diff point in different directions, explain that the league environment changed around the player.",
    "Cover core skills and luck/regression indicators in prose, without repetition.",
    "Treat PA/TBF and sample size as major confidence inputs; small samples require caution.",
    "Use age in the rest-of-season judgment when it changes the interpretation: younger players get more development upside, older players get less benefit of the doubt, and prime-age players need stronger skill evidence.",
    "Do not speculate beyond provided stats: injuries, mechanics, lineup role, or team context.",
    "No first person and no word 'thesis'.",
    `Maintain this persona throughout; tone must not override facts or sample-size caution: ${persona}`,
  ];
}

function buildMetricSelectionInstructions(type: PlayerType): string[] {
  if (type === "hitter") {
    return [
      "Metric selection guidance for hitters:",
      "- Start from overall quality indicators: xwOBA and wOBA, especially when they disagree.",
      "- Use K%, BB%, and Barrel% to explain whether the change is skill-based.",
      "- Use BABIP and the xwOBA-wOBA gap for luck/regression; do not treat AVG/OBP/SLG as the whole story when skill indicators conflict.",
      "- AVG, OBP, and SLG are supporting context, not automatic lead metrics.",
    ];
  }

  return [
    "Metric selection guidance for pitchers:",
    "- Start from run-prevention quality and sustainability: xERA vs ERA, K-BB%, and Barrel Rate.",
    "- Use K%, BB%, CSW%, and O-Swing% to explain whether the skill change is real.",
    "- Use BABIP, LOB%, and ERA/xERA gaps for luck/regression; do not lead with ERA alone when estimators or skills disagree.",
    "- Role and TBF should shape confidence: reliever samples need more caution than starter samples.",
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
    "",
    ...buildMetricSelectionInstructions(type),
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
      return [
        `${hitter.Name}:`,
        `  Age: ${formatStatValue(hitter.Age, 0)}`,
        `  PA: ${formatStatValue(hitter.PA_cur, 0)}`,
        `  ${formatMetricLine("AVG", formatStatValue(hitter.AVG_cur), formatStatValue(hitter.AVG_l3), formatStatValue(hitter.AVG_diff), formatStatValue(hitter.AVG_lg_adj_diff))}`,
        `  ${formatMetricLine("OBP", formatStatValue(hitter.OBP_cur), formatStatValue(hitter.OBP_l3), formatStatValue(hitter.OBP_diff), formatStatValue(hitter.OBP_lg_adj_diff))}`,
        `  ${formatMetricLine("SLG", formatStatValue(hitter.SLG_cur), formatStatValue(hitter.SLG_l3), formatStatValue(hitter.SLG_diff), formatStatValue(hitter.SLG_lg_adj_diff))}`,
        `  ${formatMetricLine("wOBA", formatStatValue(hitter.wOBA_cur), formatStatValue(hitter.wOBA_l3), formatStatValue(hitter.wOBA_diff), formatStatValue(hitter.wOBA_lg_adj_diff))}`,
        `  ${formatMetricLine("xwOBA", formatStatValue(hitter.xwOBA_cur), formatStatValue(hitter.xwOBA_l3), formatStatValue(hitter.xwOBA_diff), formatStatValue(hitter.xwOBA_lg_adj_diff))}`,
        `  ${formatMetricLine("K%", formatPercentage(hitter.K_pct_cur), formatPercentage(hitter.K_pct_l3), formatPercentage(hitter.K_pct_diff), formatPercentage(hitter.K_pct_lg_adj_diff))}`,
        `  ${formatMetricLine("BB%", formatPercentage(hitter.BB_pct_cur), formatPercentage(hitter.BB_pct_l3), formatPercentage(hitter.BB_pct_diff), formatPercentage(hitter.BB_pct_lg_adj_diff))}`,
      ].join("\n");
    }
    const pitcher = player as PitcherRecord;
    return [
      `${pitcher.Name}:`,
      `  Age: ${formatStatValue(pitcher.Age, 0)}`,
      `  TBF: ${formatStatValue(pitcher.tbf, 0)}`,
      `  ${formatMetricLine("ERA", formatEra(pitcher.era_cur), formatEra(pitcher.era_l3), formatEra(pitcher.era_diff), formatEra(pitcher.era_lg_adj_diff))}`,
      `  ${formatMetricLine("xERA", formatEra(pitcher.xera_cur), formatEra(pitcher.xera_l3), formatEra(pitcher.xera_diff), formatEra(pitcher.xera_lg_adj_diff))}`,
      `  ${formatMetricLine("K%", formatPercentage(pitcher.k_percent_cur), formatPercentage(pitcher.k_percent_l3), formatPercentage(pitcher.k_percent_diff), formatPercentage(pitcher.k_percent_lg_adj_diff))}`,
      `  ${formatMetricLine("BB%", formatPercentage(pitcher.bb_percent_cur), formatPercentage(pitcher.bb_percent_l3), formatPercentage(pitcher.bb_percent_diff), formatPercentage(pitcher.bb_percent_lg_adj_diff))}`,
      `  ${formatMetricLine("K-BB%", formatPercentage(pitcher.k_minus_bb_percent_cur), formatPercentage(pitcher.k_minus_bb_percent_l3), formatPercentage(pitcher.k_minus_bb_percent_diff), formatPercentage(pitcher.k_minus_bb_percent_lg_adj_diff))}`,
      `  ${formatMetricLine("BABIP", formatStatValue(pitcher.babip_cur), formatStatValue(pitcher.babip_l3), formatStatValue(pitcher.babip_diff), formatStatValue(pitcher.babip_lg_adj_diff))}`,
      `  ${formatMetricLine("CSW%", formatPercentage(pitcher.csw_percent_cur), formatPercentage(pitcher.csw_percent_l3), formatPercentage(pitcher.csw_percent_diff), formatPercentage(pitcher.csw_percent_lg_adj_diff))}`,
    ].join("\n");
  });

  return [
    "Rank these players by their likelihood to perform best for the rest of the season.",
    "Provide concise reasoning in prose; do not copy the metric rows into the answer.",
    "Do not write semicolon-separated stat dumps, 'critical lines' lists, or long strings of raw metrics.",
    "Metric format: current | WB (weighted baseline) | diff | diff from league-wide weighted baseline.",
    "",
    ...buildGeneralInstructions(persona),
    "",
    ...buildMetricSelectionInstructions(type),
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
    if ((hitter.AVG_lg_adj_diff ?? hitter.AVG_diff ?? 0) > 0.02) positives.push("batting average is up relative to the league");
    if ((hitter.K_pct_lg_adj_diff ?? hitter.K_pct_diff ?? 0) < -2) positives.push("strikeouts are down relative to the league");
    if ((hitter.BB_pct_lg_adj_diff ?? hitter.BB_pct_diff ?? 0) > 1.5) positives.push("walks are up relative to the league");
    if ((hitter.Barrel_pct_lg_adj_diff ?? hitter.Barrel_pct_diff ?? 0) > 2) positives.push("hard contact is up relative to the league");

    if ((hitter.AVG_lg_adj_diff ?? hitter.AVG_diff ?? 0) < -0.02) negatives.push("batting average is down relative to the league");
    if ((hitter.K_pct_lg_adj_diff ?? hitter.K_pct_diff ?? 0) > 2) negatives.push("strikeouts are up relative to the league");
    if ((hitter.BB_pct_lg_adj_diff ?? hitter.BB_pct_diff ?? 0) < -1.5) negatives.push("walks are down relative to the league");
    if ((hitter.Barrel_pct_lg_adj_diff ?? hitter.Barrel_pct_diff ?? 0) < -2) negatives.push("hard contact is down relative to the league");
  } else {
    const pitcher = player as PitcherRecord;
    if ((pitcher.era_lg_adj_diff ?? pitcher.era_diff ?? 0) < -0.5) positives.push("ERA is down relative to the league");
    if ((pitcher.k_percent_lg_adj_diff ?? pitcher.k_percent_diff ?? 0) > 2) positives.push("strikeouts are up relative to the league");
    if ((pitcher.bb_percent_lg_adj_diff ?? pitcher.bb_percent_diff ?? 0) < -1.5) positives.push("walks allowed are down relative to the league");
    if ((pitcher.barrel_percent_lg_adj_diff ?? pitcher.barrel_percent_diff ?? 0) < -1) positives.push("hard contact allowed is down relative to the league");

    if ((pitcher.era_lg_adj_diff ?? pitcher.era_diff ?? 0) > 0.5) negatives.push("ERA is up relative to the league");
    if ((pitcher.k_percent_lg_adj_diff ?? pitcher.k_percent_diff ?? 0) < -2) negatives.push("strikeouts are down relative to the league");
    if ((pitcher.bb_percent_lg_adj_diff ?? pitcher.bb_percent_diff ?? 0) > 1.5) negatives.push("walks allowed are up relative to the league");
    if ((pitcher.barrel_percent_lg_adj_diff ?? pitcher.barrel_percent_diff ?? 0) > 1) negatives.push("hard contact allowed is up relative to the league");
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
      ? Math.abs((player as HitterRecord).BABIP_lg_adj_diff ?? (player as HitterRecord).BABIP_diff ?? 0) + Math.abs((player as HitterRecord).xwOBA_lg_adj_diff ?? (player as HitterRecord).xwOBA_diff ?? 0)
      : Math.abs((player as PitcherRecord).era_lg_adj_diff ?? (player as PitcherRecord).era_diff ?? 0) + Math.abs((player as PitcherRecord).babip_lg_adj_diff ?? (player as PitcherRecord).babip_diff ?? 0);

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
