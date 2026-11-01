import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { fireEvent, render, screen } from "@testing-library/react";
import { MemoryRouter } from "react-router-dom";
import { afterEach, describe, expect, it, vi } from "vitest";
import KeepGoing from "../KeepGoing";
import * as api from "../../api";
import type { RelatedPlayers } from "../../types";

const related: RelatedPlayers = {
  similarGap: [
    { id: "2", name: "Mike Trout", type: "hitter", gap: 0.027 },
    { id: "3", name: "Shohei Ohtani", type: "hitter", gap: -0.021 },
  ],
  rival: { id: "4", name: "Juan Soto", type: "hitter", gap: 0.01 },
};

function renderKeepGoing(onSelectPlayer = vi.fn()) {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
  render(
    <QueryClientProvider client={client}>
      <MemoryRouter>
        <KeepGoing playerId="1" playerType="hitter" onSelectPlayer={onSelectPlayer} />
      </MemoryRouter>
    </QueryClientProvider>
  );
  return { onSelectPlayer };
}

describe("KeepGoing", () => {
  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("offers similar-luck-gap players and a rival compare", async () => {
    vi.spyOn(api, "fetchRelatedPlayers").mockResolvedValue(related);
    renderKeepGoing();

    expect(await screen.findByRole("button", { name: /Similar luck gap: Mike Trout \(\+\.027\)/ })).toBeInTheDocument();
    expect(screen.getByRole("button", { name: /Similar luck gap: Shohei Ohtani \(−\.021\)/ })).toBeInTheDocument();
    expect(screen.getByRole("link", { name: "Compare with Juan Soto" })).toHaveAttribute(
      "href",
      "/?mode=compare&playerType=hitter&playerIds=1%2C4"
    );
    expect(screen.getByText(/expected wOBA minus actual wOBA/)).toBeInTheDocument();
  });

  it("opens a suggested player when its chip is chosen", async () => {
    vi.spyOn(api, "fetchRelatedPlayers").mockResolvedValue(related);
    const { onSelectPlayer } = renderKeepGoing();

    fireEvent.click(await screen.findByRole("button", { name: /Mike Trout/ }));
    expect(onSelectPlayer).toHaveBeenCalledWith("2");
  });

  it("renders nothing when there are no suggestions or the request fails", async () => {
    const spy = vi.spyOn(api, "fetchRelatedPlayers").mockResolvedValue({ similarGap: [], rival: null });
    renderKeepGoing();
    await vi.waitFor(() => expect(spy).toHaveBeenCalled());
    expect(screen.queryByRole("group", { name: "Keep going" })).not.toBeInTheDocument();
  });

  it("stays out of the way when the request fails", async () => {
    const spy = vi.spyOn(api, "fetchRelatedPlayers").mockRejectedValue(new Error("boom"));
    renderKeepGoing();
    await vi.waitFor(() => expect(spy).toHaveBeenCalled());
    expect(screen.queryByRole("group", { name: "Keep going" })).not.toBeInTheDocument();
  });
});
