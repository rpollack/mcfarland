import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { fireEvent, render, screen, within } from "@testing-library/react";
import { afterEach, describe, expect, it, vi } from "vitest";
import MirageBoard from "../MirageBoard";
import * as api from "../../api";
import { VibeContext } from "../../contexts/vibe";
import type { MirageBoard as MirageBoardData } from "../../types";

const board: MirageBoardData = {
  season: 2026,
  playerType: "hitter",
  lucky: [
    { id: "1", name: "Nolan Arenado", mlbamid: "571448", actual: 0.349, expected: 0.3, gap: -0.049 },
    { id: "2", name: "Ty France", mlbamid: "664034", actual: 0.373, expected: 0.327, gap: -0.046 },
  ],
  unlucky: [{ id: "3", name: "J.T. Realmuto", mlbamid: "592663", actual: 0.275, expected: 0.317, gap: 0.042 }],
};

function renderBoard(onSelectPlayer = vi.fn(), playerType: "hitter" | "pitcher" = "hitter") {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
  render(
    <QueryClientProvider client={client}>
      <VibeContext.Provider
        value={{
          vibes: [{ id: "gen_z", label: "Gen Z", description: "" }],
          mode: "gen_z",
          setMode: () => {},
          isLoading: false,
          defaultMode: "straightforward",
        }}
      >
        <MirageBoard playerType={playerType} onSelectPlayer={onSelectPlayer} />
      </VibeContext.Provider>
    </QueryClientProvider>
  );
  return { onSelectPlayer };
}

describe("MirageBoard", () => {
  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("shows both lists with each player's actual and expected numbers", async () => {
    vi.spyOn(api, "fetchMirageBoard").mockResolvedValue(board);
    vi.spyOn(api, "fetchMirageBoardIntro").mockResolvedValue({
      prompt: "",
      persona: "",
      headline: "Luck evens out",
      analysis: "First paragraph.\n\nSecond paragraph.",
      cached: false,
    });
    renderBoard();

    expect(await screen.findByRole("heading", { name: "2026 mirages" })).toBeInTheDocument();
    const lucky = screen.getByRole("region", { name: "Ran lucky" });
    expect(within(lucky).getByText("Nolan Arenado")).toBeInTheDocument();
    expect(within(lucky).getByText("wOBA .349, expected .300")).toBeInTheDocument();
    expect(within(lucky).getByText("−.049")).toBeInTheDocument();
    const unlucky = screen.getByRole("region", { name: "Ran unlucky" });
    expect(within(unlucky).getByText("+.042")).toBeInTheDocument();
  });

  it("shows the intro in the chosen voice", async () => {
    vi.spyOn(api, "fetchMirageBoard").mockResolvedValue(board);
    const intro = vi.spyOn(api, "fetchMirageBoardIntro").mockResolvedValue({
      prompt: "",
      persona: "",
      headline: "Luck evens out",
      analysis: "First paragraph.\n\nSecond paragraph.",
      cached: false,
    });
    renderBoard();

    expect(await screen.findByRole("heading", { name: "Luck evens out" })).toBeInTheDocument();
    expect(screen.getByText("First paragraph.")).toBeInTheDocument();
    expect(screen.getByText("Second paragraph.")).toBeInTheDocument();
    expect(screen.getByText("Written in the Gen Z voice.")).toBeInTheDocument();
    expect(intro).toHaveBeenCalledWith("hitter", "gen_z");
  });

  it("still shows the board when the intro cannot be written", async () => {
    vi.spyOn(api, "fetchMirageBoard").mockResolvedValue(board);
    vi.spyOn(api, "fetchMirageBoardIntro").mockRejectedValue(new Error("down"));
    renderBoard();

    expect(await screen.findByRole("region", { name: "Ran lucky" })).toBeInTheDocument();
    await vi.waitFor(() => expect(screen.queryByText(/Written in/)).not.toBeInTheDocument());
    expect(screen.queryByRole("heading", { name: "Luck evens out" })).not.toBeInTheDocument();
  });

  it("opens a player when their row is chosen", async () => {
    vi.spyOn(api, "fetchMirageBoard").mockResolvedValue(board);
    vi.spyOn(api, "fetchMirageBoardIntro").mockRejectedValue(new Error("down"));
    const { onSelectPlayer } = renderBoard();

    fireEvent.click(await screen.findByRole("button", { name: /J\.T\. Realmuto/ }));
    expect(onSelectPlayer).toHaveBeenCalledWith("3");
  });

  it("describes pitchers in ERA and xERA", async () => {
    vi.spyOn(api, "fetchMirageBoard").mockResolvedValue({
      season: 2026,
      playerType: "pitcher",
      lucky: [{ id: "9", name: "Some Pitcher", mlbamid: null, actual: 1.8, expected: 2.12, gap: -0.32 }],
      unlucky: [],
    });
    vi.spyOn(api, "fetchMirageBoardIntro").mockRejectedValue(new Error("down"));
    renderBoard(vi.fn(), "pitcher");

    expect(await screen.findByText("ERA 1.80, expected 2.12")).toBeInTheDocument();
    expect(screen.getByText("−.32".replace(".", "0."))).toBeInTheDocument();
  });

  it("renders nothing when there is no board", async () => {
    const spy = vi
      .spyOn(api, "fetchMirageBoard")
      .mockResolvedValue({ season: 2026, playerType: "hitter", lucky: [], unlucky: [] });
    renderBoard();

    await vi.waitFor(() => expect(spy).toHaveBeenCalled());
    expect(screen.queryByRole("region", { name: "Mirage board" })).not.toBeInTheDocument();
  });
});
