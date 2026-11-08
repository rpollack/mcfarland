import { fireEvent, render, screen } from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import StarButton from "../StarButton";
import * as api from "../../api";
import { __resetSessionStarredForTests, isPlayerStarred } from "../../utils/starredPlayers";

describe("StarButton", () => {
  let logSpy: ReturnType<typeof vi.spyOn>;

  beforeEach(() => {
    window.localStorage.clear();
    __resetSessionStarredForTests();
    logSpy = vi.spyOn(api, "logUiEvent").mockResolvedValue();
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("logs stars and unstars with the new state", () => {
    render(<StarButton playerId="15640" playerName="Aaron Judge" playerType="hitter" />);

    expect(logSpy).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole("button", { name: "Star Aaron Judge" }));
    expect(logSpy).toHaveBeenLastCalledWith({
      event: "player_star_toggled",
      playerType: "hitter",
      playerId: "15640",
      playerName: "Aaron Judge",
      starred: true,
    });

    fireEvent.click(screen.getByRole("button", { name: /Remove Aaron Judge/ }));
    expect(logSpy).toHaveBeenLastCalledWith(expect.objectContaining({ event: "player_star_toggled", starred: false }));
    expect(logSpy).toHaveBeenCalledTimes(2);
  });

  it("stars and unstars with accessible labels", () => {
    render(<StarButton playerId="15640" playerName="Aaron Judge" playerType="hitter" />);

    const button = screen.getByRole("button", { name: "Star Aaron Judge" });
    expect(button).toHaveAttribute("aria-pressed", "false");

    fireEvent.click(button);
    expect(isPlayerStarred("15640", "hitter")).toBe(true);
    expect(screen.getByRole("button", { name: "Remove Aaron Judge from your starred players" })).toHaveAttribute(
      "aria-pressed",
      "true"
    );

    fireEvent.click(screen.getByRole("button", { name: /Remove Aaron Judge/ }));
    expect(isPlayerStarred("15640", "hitter")).toBe(false);
  });

  it("shows a player that was starred earlier as starred", () => {
    const { unmount } = render(<StarButton playerId="15640" playerName="Aaron Judge" playerType="hitter" />);
    fireEvent.click(screen.getByRole("button", { name: "Star Aaron Judge" }));
    unmount();

    render(<StarButton playerId="15640" playerName="Aaron Judge" playerType="hitter" />);
    expect(screen.getByRole("button", { name: /Remove Aaron Judge/ })).toHaveAttribute("aria-pressed", "true");
  });

  it("updates when the shown player changes", () => {
    const { rerender } = render(<StarButton playerId="15640" playerName="Aaron Judge" playerType="hitter" />);
    fireEvent.click(screen.getByRole("button", { name: "Star Aaron Judge" }));

    rerender(<StarButton playerId="7" playerName="Someone Else" playerType="hitter" />);
    expect(screen.getByRole("button", { name: "Star Someone Else" })).toHaveAttribute("aria-pressed", "false");
  });
});
