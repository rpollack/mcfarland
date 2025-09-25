import { fireEvent, render, screen } from "@testing-library/react";
import PlayerPicker from "../PlayerPicker";
import type { PlayerSummary } from "../../types";

const players: PlayerSummary[] = [
  { id: "1", name: "Player One", type: "hitter" },
  { id: "2", name: "Player Two", type: "hitter" },
];

describe("PlayerPicker", () => {
  it("allows switching player types", () => {
    const handleTypeChange = vi.fn();
    render(
      <PlayerPicker
        playerType="hitter"
        onTypeChange={handleTypeChange}
        searchTerm=""
        onSearchTermChange={() => {}}
        players={players}
        selectedId="1"
        onSelect={() => {}}
        isLoading={false}
      />
    );

    fireEvent.click(screen.getByRole("button", { name: /pitchers/i }));
    expect(handleTypeChange).toHaveBeenCalledWith("pitcher");
  });
});
