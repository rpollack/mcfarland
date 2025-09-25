import { render, screen } from "@testing-library/react";
import { describe, expect, it } from "vitest";
import PlayerStatsCard from "../PlayerStatsCard";
import type { HitterRecord } from "../../types";

const hitter: HitterRecord = {
  Name: "Test Hitter",
  PlayerId: "1",
  Age: 28,
  AVG_cur: 0.312,
  OBP_cur: 0.401,
  SLG_cur: 0.55,
  K_pct_cur: 20,
  BB_pct_cur: 10,
  Barrel_pct_cur: 12,
  BABIP_cur: 0.32,
  wOBA_cur: 0.4,
  xwOBA_cur: 0.41,
  xwOBA_wOBA_gap_cur: 0.01,
  PA_cur: 250,
  AVG_l3: 0.28,
  OBP_l3: 0.36,
  SLG_l3: 0.49,
  K_pct_l3: 22,
  BB_pct_l3: 8,
  Barrel_pct_l3: 10,
  BABIP_l3: 0.3,
  wOBA_l3: 0.37,
  xwOBA_l3: 0.38,
  xwOBA_wOBA_gap_l3: 0.01,
  PA_l3: 1800,
  AVG_diff: 0.032,
  OBP_diff: 0.041,
  SLG_diff: 0.06,
  K_pct_diff: -2,
  BB_pct_diff: 2,
  Barrel_pct_diff: 2,
  BABIP_diff: 0.02,
  wOBA_diff: 0.03,
  xwOBA_diff: 0.03,
  xwOBA_wOBA_gap_diff: 0,
};

describe("PlayerStatsCard", () => {
  it("renders key hitter metrics", () => {
    render(<PlayerStatsCard type="hitter" player={hitter} />);
    expect(screen.getByRole("heading", { level: 2 })).toHaveTextContent("Test Hitter");
    expect(screen.getByText("AVG")).toBeInTheDocument();
    expect(screen.getByText("0.312")).toBeInTheDocument();
    expect(screen.getByText("0.401")).toBeInTheDocument();
  });
});
