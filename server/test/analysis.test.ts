import { describe, expect, it } from "vitest";
import { buildHitterPrompt, buildPitcherPrompt } from "../src/analysis.js";
import type { HitterRecord, PitcherRecord } from "../src/types.js";

describe("analysis prompts", () => {
  it("labels hitter baselines as weighted baseline", () => {
    const prompt = buildHitterPrompt({
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
    } satisfies HitterRecord);

    expect(prompt).toContain("weighted baseline");
    expect(prompt).not.toContain("Last 3 Years");
  });

  it("labels pitcher baselines as weighted baseline", () => {
    const prompt = buildPitcherPrompt({
      Name: "Test Pitcher",
      PlayerId: "2",
      Age: 29,
      tbf: 220,
      babip_cur: 0.29,
      lob_percent_cur: 72,
      ld_percent_cur: 19,
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
      ld_percent_l3: 20,
      o_swing_percent_l3: 29,
      babip_l3: 0.3,
      lob_percent_l3: 74,
      csw_percent_l3: 27,
      era_diff: -0.4,
      k_percent_diff: 1,
      bb_percent_diff: 1,
      k_minus_bb_percent_diff: 0,
      xera_diff: -0.2,
      ld_percent_diff: -1,
      o_swing_percent_diff: 1,
      csw_percent_diff: 1,
      barrel_percent_diff: -1,
      lob_percent_diff: -2,
      babip_diff: -0.01,
      player_type: "pitcher",
    } satisfies PitcherRecord);

    expect(prompt).toContain("weighted baseline");
    expect(prompt).not.toContain("Last 3 Years");
  });
});
