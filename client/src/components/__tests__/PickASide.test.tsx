import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { fireEvent, render, screen } from "@testing-library/react";
import { afterEach, describe, expect, it, vi } from "vitest";
import PickASide from "../PickASide";
import * as api from "../../api";

const players = [
  { id: "1", name: "Aaron Judge" },
  { id: "2", name: "Shohei Ohtani" },
];

function renderPickASide() {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
  render(
    <QueryClientProvider client={client}>
      <PickASide playerType="hitter" players={players} />
    </QueryClientProvider>
  );
}

describe("PickASide", () => {
  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("asks who you would take and hides the split until you vote", async () => {
    vi.spyOn(api, "fetchCompareVotes").mockResolvedValue({ yourVote: null, counts: null, total: 0 });
    renderPickASide();

    expect(await screen.findByRole("group", { name: "Who would you take?" })).toBeInTheDocument();
    expect(screen.getByRole("button", { name: "Aaron Judge" })).toBeInTheDocument();
    expect(screen.getByRole("button", { name: "Shohei Ohtani" })).toBeInTheDocument();
    expect(screen.queryByText(/%/)).not.toBeInTheDocument();
  });

  it("casts a vote and then shows the split with your pick marked", async () => {
    vi.spyOn(api, "fetchCompareVotes").mockResolvedValue({ yourVote: null, counts: null, total: 0 });
    const cast = vi
      .spyOn(api, "castCompareVote")
      .mockResolvedValue({ yourVote: "1", counts: { "1": 3, "2": 1 }, total: 4 });
    renderPickASide();

    fireEvent.click(await screen.findByRole("button", { name: "Aaron Judge" }));

    expect(await screen.findByRole("group", { name: "How people voted" })).toBeInTheDocument();
    expect(cast).toHaveBeenCalledWith("hitter", ["1", "2"], "1");
    expect(screen.getByText("75%")).toBeInTheDocument();
    expect(screen.getByText("25%")).toBeInTheDocument();
    expect(screen.getByText(/your pick/)).toBeInTheDocument();
    expect(screen.getByText("4 votes so far.")).toBeInTheDocument();
  });

  it("shows the results straight away for someone who already voted", async () => {
    vi.spyOn(api, "fetchCompareVotes").mockResolvedValue({ yourVote: "2", counts: { "2": 1 }, total: 1 });
    renderPickASide();

    expect(await screen.findByText(/first vote on this matchup/)).toBeInTheDocument();
    expect(screen.queryByRole("button", { name: "Aaron Judge" })).not.toBeInTheDocument();
  });

  it("renders nothing when voting is unavailable", async () => {
    const spy = vi.spyOn(api, "fetchCompareVotes").mockRejectedValue(new Error("down"));
    renderPickASide();

    await vi.waitFor(() => expect(spy).toHaveBeenCalled());
    expect(screen.queryByRole("group")).not.toBeInTheDocument();
  });
});
