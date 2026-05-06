import { HitterRecord, PitcherRecord, PlayerType } from "./types.js";
import { CURRENT_YEAR, formatEra, formatPercentage, formatStatValue } from "./utils.js";
import { ANALYSIS_VIBES, AnalysisMode } from "./vibes.js";

export function buildHitterPrompt(player: HitterRecord): string {
  const header = [
    `Player: ${player.Name} (Hitter)`,
    "",
    "--- Key metrics to analyze ---",
    "Format: current | WB (weighted baseline) | diff",
    `Age: ${formatStatValue(player.Age, 1)}`,
    `Year: ${CURRENT_YEAR}`,
    `Plate Appearances (PA): ${formatStatValue(player.PA_cur, 0)}`,
    "",
  ];

  const metrics = [
    `AVG: ${formatStatValue(player.AVG_cur)} | WB ${formatStatValue(player.AVG_l3)} | diff ${formatStatValue(player.AVG_diff)}`,
    `OBP: ${formatStatValue(player.OBP_cur)} | WB ${formatStatValue(player.OBP_l3)} | diff ${formatStatValue(player.OBP_diff)}`,
    `SLG: ${formatStatValue(player.SLG_cur)} | WB ${formatStatValue(player.SLG_l3)} | diff ${formatStatValue(player.SLG_diff)}`,
    `K%: ${formatPercentage(player.K_pct_cur)} | WB ${formatPercentage(player.K_pct_l3)} | diff ${formatPercentage(player.K_pct_diff)}`,
    `BB%: ${formatPercentage(player.BB_pct_cur)} | WB ${formatPercentage(player.BB_pct_l3)} | diff ${formatPercentage(player.BB_pct_diff)}`,
    `Barrel%: ${formatPercentage(player.Barrel_pct_cur)} | WB ${formatPercentage(player.Barrel_pct_l3)} | diff ${formatPercentage(player.Barrel_pct_diff)}`,
    `BABIP: ${formatStatValue(player.BABIP_cur)} | WB ${formatStatValue(player.BABIP_l3)} | diff ${formatStatValue(player.BABIP_diff)}`,
    `wOBA: ${formatStatValue(player.wOBA_cur)} | WB ${formatStatValue(player.wOBA_l3)} | diff ${formatStatValue(player.wOBA_diff)}`,
    `xwOBA: ${formatStatValue(player.xwOBA_cur)} | WB ${formatStatValue(player.xwOBA_l3)} | diff ${formatStatValue(player.xwOBA_diff)}`,
    `xwOBA-wOBA gap: ${formatStatValue(player.xwOBA_wOBA_gap_cur)} | WB ${formatStatValue(player.xwOBA_wOBA_gap_l3)} | diff ${formatStatValue(player.xwOBA_wOBA_gap_diff)}`,
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
    "Format: current | WB (weighted baseline) | diff",
    `Age: ${formatStatValue(player.Age, 1)}`,
    `Year: ${CURRENT_YEAR}`,
    `Position: ${player.position ?? "Pitcher"}`,
    `Total Batters Faced: ${formatStatValue(player.tbf, 0)}`,
    "",
  ];

  const metrics = [
    `ERA: ${formatEra(player.era_cur)} | WB ${formatEra(player.era_l3)} | diff ${formatEra(player.era_diff)}`,
    `xERA: ${formatEra(player.xera_cur)} | WB ${formatEra(player.xera_l3)} | diff ${formatEra(player.xera_diff)}`,
    `BABIP: ${formatStatValue(player.babip_cur)} | WB ${formatStatValue(player.babip_l3)} | diff ${formatStatValue(player.babip_diff)}`,
    `Barrel Rate: ${formatPercentage(player.barrel_percent_cur)} | WB ${formatPercentage(player.barrel_percent_l3)} | diff ${formatPercentage(player.barrel_percent_diff)}`,
    `Strikeout Rate (K%): ${formatPercentage(player.k_percent_cur)} | WB ${formatPercentage(player.k_percent_l3)} | diff ${formatPercentage(player.k_percent_diff)}`,
    `Called Strike & Whiff Rate (CSW%): ${formatPercentage(player.csw_percent_cur)} | WB ${formatPercentage(player.csw_percent_l3)} | diff ${formatPercentage(player.csw_percent_diff)}`,
    `Outside Zone Swing Rate (O-Swing%): ${formatPercentage(player.o_swing_percent_cur)} | WB ${formatPercentage(player.o_swing_percent_l3)} | diff ${formatPercentage(player.o_swing_percent_diff)}`,
    `Walk Rate (BB%): ${formatPercentage(player.bb_percent_cur)} | WB ${formatPercentage(player.bb_percent_l3)} | diff ${formatPercentage(player.bb_percent_diff)}`,
    `K-BB%: ${formatPercentage(player.k_minus_bb_percent_cur)} | WB ${formatPercentage(player.k_minus_bb_percent_l3)} | diff ${formatPercentage(player.k_minus_bb_percent_diff)}`,
    `LOB%: ${formatPercentage(player.lob_percent_cur)} | WB ${formatPercentage(player.lob_percent_l3)} | diff ${formatPercentage(player.lob_percent_diff)}`,
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
    "Include each selected metric's direction and magnitude vs weighted baseline.",
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
        `  Age: ${formatStatValue(hitter.Age, 1)}`,
        `  PA: ${formatStatValue(hitter.PA_cur, 0)}`,
        `  AVG: ${formatStatValue(hitter.AVG_cur)} | WB ${formatStatValue(hitter.AVG_l3)} | diff ${formatStatValue(hitter.AVG_diff)}`,
        `  OBP: ${formatStatValue(hitter.OBP_cur)} | WB ${formatStatValue(hitter.OBP_l3)} | diff ${formatStatValue(hitter.OBP_diff)}`,
        `  SLG: ${formatStatValue(hitter.SLG_cur)} | WB ${formatStatValue(hitter.SLG_l3)} | diff ${formatStatValue(hitter.SLG_diff)}`,
        `  wOBA: ${formatStatValue(hitter.wOBA_cur)} | WB ${formatStatValue(hitter.wOBA_l3)} | diff ${formatStatValue(hitter.wOBA_diff)}`,
        `  xwOBA: ${formatStatValue(hitter.xwOBA_cur)} | WB ${formatStatValue(hitter.xwOBA_l3)} | diff ${formatStatValue(hitter.xwOBA_diff)}`,
        `  K%: ${formatPercentage(hitter.K_pct_cur)} | WB ${formatPercentage(hitter.K_pct_l3)} | diff ${formatPercentage(hitter.K_pct_diff)}`,
        `  BB%: ${formatPercentage(hitter.BB_pct_cur)} | WB ${formatPercentage(hitter.BB_pct_l3)} | diff ${formatPercentage(hitter.BB_pct_diff)}`,
      ].join("\n");
    }
    const pitcher = player as PitcherRecord;
    return [
      `${pitcher.Name}:`,
      `  Age: ${formatStatValue(pitcher.Age, 1)}`,
      `  TBF: ${formatStatValue(pitcher.tbf, 0)}`,
      `  ERA: ${formatEra(pitcher.era_cur)} | WB ${formatEra(pitcher.era_l3)} | diff ${formatEra(pitcher.era_diff)}`,
      `  xERA: ${formatEra(pitcher.xera_cur)} | WB ${formatEra(pitcher.xera_l3)} | diff ${formatEra(pitcher.xera_diff)}`,
      `  K%: ${formatPercentage(pitcher.k_percent_cur)} | WB ${formatPercentage(pitcher.k_percent_l3)} | diff ${formatPercentage(pitcher.k_percent_diff)}`,
      `  BB%: ${formatPercentage(pitcher.bb_percent_cur)} | WB ${formatPercentage(pitcher.bb_percent_l3)} | diff ${formatPercentage(pitcher.bb_percent_diff)}`,
      `  K-BB%: ${formatPercentage(pitcher.k_minus_bb_percent_cur)} | WB ${formatPercentage(pitcher.k_minus_bb_percent_l3)} | diff ${formatPercentage(pitcher.k_minus_bb_percent_diff)}`,
      `  BABIP: ${formatStatValue(pitcher.babip_cur)} | WB ${formatStatValue(pitcher.babip_l3)} | diff ${formatStatValue(pitcher.babip_diff)}`,
      `  CSW%: ${formatPercentage(pitcher.csw_percent_cur)} | WB ${formatPercentage(pitcher.csw_percent_l3)} | diff ${formatPercentage(pitcher.csw_percent_diff)}`,
    ].join("\n");
  });

  return [
    "Rank these players by their likelihood to perform best for the rest of the season.",
    "Provide concise reasoning in prose; do not copy the metric rows into the answer.",
    "Do not write semicolon-separated stat dumps, 'critical lines' lists, or long strings of raw metrics.",
    "Metric format: current | WB (weighted baseline) | diff.",
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
