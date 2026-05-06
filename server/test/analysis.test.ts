import { describe, expect, it } from "vitest";
import { buildAnalysisPrompt, buildComparisonPrompt, buildHitterPrompt, buildPitcherPrompt } from "../src/analysis.js";
import type { HitterRecord, PitcherRecord } from "../src/types.js";

describe("analysis prompts", () => {
  it("labels hitter baselines as weighted baseline", () => {
    const hitter = {
      Name: "Test Hitter",
      PlayerId: "1",
      Age: 28,
      AVG_cur: 0.3,
      OBP_cur: 0.37,
      SLG_cur: 0.5,
      K_pct_cur: 22,
      BB_pct_cur: 9,
      Barrel_pct_cur: 10,
      BABIP_cur: 0.31,
      wOBA_cur: 0.36,
      xwOBA_cur: 0.37,
      xwOBA_wOBA_gap_cur: 0.01,
      PA_cur: 240,
      AVG_l3: 0.28,
      OBP_l3: 0.35,
      SLG_l3: 0.47,
      K_pct_l3: 24,
      BB_pct_l3: 8,
      Barrel_pct_l3: 9,
      BABIP_l3: 0.3,
      wOBA_l3: 0.34,
      xwOBA_l3: 0.35,
      xwOBA_wOBA_gap_l3: 0.01,
      PA_l3: 1600,
      AVG_diff: 0.02,
      OBP_diff: 0.02,
      SLG_diff: 0.03,
      K_pct_diff: -2,
      BB_pct_diff: 1,
      Barrel_pct_diff: 1,
      BABIP_diff: 0.01,
      wOBA_diff: 0.02,
      xwOBA_diff: 0.02,
      xwOBA_wOBA_gap_diff: 0,
      player_type: "hitter",
    } satisfies HitterRecord;

    const prompt = buildHitterPrompt(hitter);
    expect(prompt).toContain("weighted baseline");
    expect(prompt).not.toContain("Last 3 Years");

    const analysisPrompt = buildAnalysisPrompt(hitter, "hitter", "straightforward");
    expect(analysisPrompt).toContain("Write complete, natural sentences with varied openings");
    expect(analysisPrompt).toContain("avoid metric-first fragments");
    expect(analysisPrompt).toContain("stock openers like 'the clearest read'");
    expect(analysisPrompt).toContain("Use an inverted-pyramid structure");
    expect(analysisPrompt).toContain("most important conclusion first");
    expect(analysisPrompt).toContain("3-5 short paragraphs separated by blank lines");
    expect(analysisPrompt).toContain("Choose the 2-4 most diagnostic signals");
    expect(analysisPrompt).toContain("do not default to the same metrics every time");
    expect(analysisPrompt).toContain("Use age in the rest-of-season judgment");
    expect(analysisPrompt).toContain("Metric selection guidance for hitters");
    expect(analysisPrompt).toContain("xwOBA and wOBA");
    expect(analysisPrompt).toContain("AVG, OBP, and SLG are supporting context");
  });

  it("labels pitcher baselines as weighted baseline", () => {
    const pitcher = {
      Name: "Test Pitcher",
      PlayerId: "2",
      Age: 29,
      tbf: 220,
      babip_cur: 0.29,
      lob_percent_cur: 72,
      barrel_percent_cur: 7,
      o_swing_percent_cur: 30,
      csw_percent_cur: 28,
      xera_cur: 3.5,
      k_percent_cur: 25,
      bb_percent_cur: 8,
      k_minus_bb_percent_cur: 17,
      era_cur: 3.4,
      era_l3: 3.8,
      k_percent_l3: 24,
      bb_percent_l3: 7,
      k_minus_bb_percent_l3: 17,
      xera_l3: 3.7,
      barrel_percent_l3: 8,
      o_swing_percent_l3: 29,
      babip_l3: 0.3,
      lob_percent_l3: 74,
      csw_percent_l3: 27,
      era_diff: -0.4,
      k_percent_diff: 1,
      bb_percent_diff: 1,
      k_minus_bb_percent_diff: 0,
      xera_diff: -0.2,
      o_swing_percent_diff: 1,
      csw_percent_diff: 1,
      barrel_percent_diff: -1,
      lob_percent_diff: -2,
      babip_diff: -0.01,
      player_type: "pitcher",
    } satisfies PitcherRecord;

    const prompt = buildPitcherPrompt(pitcher);

    expect(prompt).toContain("weighted baseline");
    expect(prompt).not.toContain("Last 3 Years");

    const analysisPrompt = buildAnalysisPrompt(pitcher, "pitcher", "straightforward");
    expect(analysisPrompt).toContain("Metric selection guidance for pitchers");
    expect(analysisPrompt).toContain("xERA vs ERA, K-BB%, and Barrel Rate");
    expect(analysisPrompt).toContain("Use K%, BB%, CSW%, and O-Swing%");
    expect(analysisPrompt).toContain("Use age in the rest-of-season judgment");
  });

  it("tells comparison prompts not to dump metric rows into prose", () => {
    const hitters = [
      {
        Name: "Test Hitter A",
        PlayerId: "1",
        Age: 28,
        AVG_cur: 0.3,
        OBP_cur: 0.37,
        SLG_cur: 0.5,
        K_pct_cur: 22,
        BB_pct_cur: 9,
        Barrel_pct_cur: 10,
        BABIP_cur: 0.31,
        wOBA_cur: 0.36,
        xwOBA_cur: 0.37,
        xwOBA_wOBA_gap_cur: 0.01,
        PA_cur: 240,
        AVG_l3: 0.28,
        OBP_l3: 0.35,
        SLG_l3: 0.47,
        K_pct_l3: 24,
        BB_pct_l3: 8,
        Barrel_pct_l3: 9,
        BABIP_l3: 0.3,
        wOBA_l3: 0.34,
        xwOBA_l3: 0.35,
        xwOBA_wOBA_gap_l3: 0.01,
        PA_l3: 1600,
        AVG_diff: 0.02,
        OBP_diff: 0.02,
        SLG_diff: 0.03,
        K_pct_diff: -2,
        BB_pct_diff: 1,
        Barrel_pct_diff: 1,
        BABIP_diff: 0.01,
        wOBA_diff: 0.02,
        xwOBA_diff: 0.02,
        xwOBA_wOBA_gap_diff: 0,
        player_type: "hitter",
      },
      {
        Name: "Test Hitter B",
        PlayerId: "2",
        Age: 30,
        AVG_cur: 0.24,
        OBP_cur: 0.31,
        SLG_cur: 0.39,
        K_pct_cur: 20,
        BB_pct_cur: 8,
        Barrel_pct_cur: 8,
        BABIP_cur: 0.29,
        wOBA_cur: 0.31,
        xwOBA_cur: 0.32,
        xwOBA_wOBA_gap_cur: 0.01,
        PA_cur: 230,
        AVG_l3: 0.25,
        OBP_l3: 0.32,
        SLG_l3: 0.4,
        K_pct_l3: 19,
        BB_pct_l3: 9,
        Barrel_pct_l3: 8,
        BABIP_l3: 0.3,
        wOBA_l3: 0.32,
        xwOBA_l3: 0.32,
        xwOBA_wOBA_gap_l3: 0,
        PA_l3: 1400,
        AVG_diff: -0.01,
        OBP_diff: -0.01,
        SLG_diff: -0.01,
        K_pct_diff: 1,
        BB_pct_diff: -1,
        Barrel_pct_diff: 0,
        BABIP_diff: -0.01,
        wOBA_diff: -0.01,
        xwOBA_diff: 0,
        xwOBA_wOBA_gap_diff: 0.01,
        player_type: "hitter",
      },
    ] satisfies HitterRecord[];

    const prompt = buildComparisonPrompt(hitters, "hitter", "straightforward");

    expect(prompt).toContain("do not copy the metric rows into the answer");
    expect(prompt).toContain("Do not write semicolon-separated stat dumps");
    expect(prompt).toContain("critical lines");
    expect(prompt).toContain("Choose the 2-4 most diagnostic signals");
    expect(prompt).toContain("Use age in the rest-of-season judgment");
    expect(prompt).toContain("Metric selection guidance for hitters");
    expect(prompt).toContain("  Age: 28.0");
    expect(prompt).toContain("  Age: 30.0");
  });
});
