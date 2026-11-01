import { fireEvent, render, screen } from "@testing-library/react";
import { beforeEach, describe, expect, it } from "vitest";
import StarButton from "../StarButton";
import { __resetSessionStarredForTests, isPlayerStarred } from "../../utils/starredPlayers";

describe("StarButton", () => {
  beforeEach(() => {
    window.localStorage.clear();
    __resetSessionStarredForTests();
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
