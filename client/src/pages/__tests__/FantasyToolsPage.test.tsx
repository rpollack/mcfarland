import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { fireEvent, render, screen, waitFor } from "@testing-library/react";
import type { ReactNode } from "react";
import { describe, expect, it, vi } from "vitest";
import FantasyToolsPage from "../FantasyToolsPage";
import { analyzeDailyMatchup, fetchPlayers } from "../../api";

vi.mock("../../api", () => ({
  analyzeDailyMatchup: vi.fn(),
  fetchPlayers: vi.fn(),
}));

function renderWithClient(ui: ReactNode) {
  const client = new QueryClient({
    defaultOptions: {
      queries: { retry: false },
      mutations: { retry: false },
    },
  });
  return render(<QueryClientProvider client={client}>{ui}</QueryClientProvider>);
}

describe("FantasyToolsPage", () => {
  it("selects a player and renders daily matchup recommendation", async () => {
    vi.mocked(fetchPlayers).mockResolvedValue([
      { id: "15640", name: "Aaron Judge", type: "hitter", mlbamid: "592450" },
    ]);
    vi.mocked(analyzeDailyMatchup).mockResolvedValue({
      player: {
        Name: "Aaron Judge",
        PlayerId: "15640",
        Age: 33,
        mlbamid: "592450",
        AVG_cur: 0.31,
        OBP_cur: 0.42,
        SLG_cur: 0.61,
        K_pct_cur: 24,
        BB_pct_cur: 15,
        Barrel_pct_cur: 18,
        BABIP_cur: 0.31,
        wOBA_cur: 0.43,
        xwOBA_cur: 0.45,
        xwOBA_wOBA_gap_cur: 0.02,
        PA_cur: 120,
        AVG_l3: 0.29,
        OBP_l3: 0.4,
        SLG_l3: 0.58,
        K_pct_l3: 25,
        BB_pct_l3: 14,
        Barrel_pct_l3: 17,
        BABIP_l3: 0.3,
        wOBA_l3: 0.41,
        xwOBA_l3: 0.42,
        xwOBA_wOBA_gap_l3: 0.01,
        PA_l3: 540,
        AVG_diff: 0.02,
        OBP_diff: 0.02,
        SLG_diff: 0.03,
        K_pct_diff: -1,
        BB_pct_diff: 1,
        Barrel_pct_diff: 1,
        BABIP_diff: 0.01,
        wOBA_diff: 0.02,
        xwOBA_diff: 0.03,
        xwOBA_wOBA_gap_diff: 0.01,
      },
      matchup: {
        matchupStatus: "ok",
        date: "2026-04-25",
        gamePk: 824203,
        gameDate: "2026-04-25T23:10:00Z",
        gameStatus: "Scheduled",
        homeAway: "away",
        playerTeam: { id: 147, name: "New York Yankees", abbreviation: "NYY" },
        opponent: { id: 117, name: "Houston Astros", abbreviation: "HOU" },
        venue: "Daikin Park",
        weather: { condition: "Clear", temp: "72", wind: "5 mph" },
        selectedPlayerHandedness: { batSide: "R", pitchHand: "R" },
        opposingStarter: { id: 677960, name: "Ryan Weathers", pitchHand: "L" },
        platoonLabel: "RHB vs LHP",
        lineupStatus: "confirmed",
      },
      decision: "START",
      confidence: "Medium",
      headline: "Start Judge for the platoon edge",
      analysis: "START Judge with the platoon edge.",
      prompt: "prompt",
      cached: false,
    });

    renderWithClient(
      <FantasyToolsPage initialPlayerType="hitter" onStateChange={() => {}} />
    );

    fireEvent.change(screen.getByRole("searchbox"), { target: { value: "Judge" } });
    const result = await screen.findByRole("button", { name: "Aaron Judge" });
    fireEvent.click(result);

    await waitFor(() => expect(analyzeDailyMatchup).toHaveBeenCalledWith("15640", "hitter"));
    expect(await screen.findByText("START")).toBeInTheDocument();
    expect(screen.getByText("Start Judge for the platoon edge")).toBeInTheDocument();
    expect(screen.getByText(/NYY at HOU/)).toBeInTheDocument();
    expect(screen.getByText("RHB vs LHP")).toBeInTheDocument();
  });
});
