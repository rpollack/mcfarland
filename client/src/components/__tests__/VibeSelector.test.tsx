import { fireEvent, render, screen } from "@testing-library/react";
import { describe, expect, it, vi } from "vitest";
import VibeSelector from "../VibeSelector";
import { pickSurpriseVibe } from "../../utils/vibes";
import { VibeContext } from "../../contexts/vibe";
import type { VibeContextValue } from "../../contexts/vibe";

const vibes = [
  { id: "straightforward", label: "Straightforward (Default)", description: "" },
  { id: "analytics_dork", label: "Analytics Nerd", description: "" },
  { id: "gen_z", label: "Gen Z", description: "" },
];

function renderSelector(overrides: Partial<VibeContextValue> = {}) {
  const setMode = vi.fn();
  render(
    <VibeContext.Provider
      value={{ vibes, mode: "straightforward", setMode, isLoading: false, defaultMode: "straightforward", ...overrides }}
    >
      <VibeSelector />
    </VibeContext.Provider>
  );
  return { setMode };
}

describe("VibeSelector", () => {
  it("shows every vibe as a chip and marks the current one", () => {
    renderSelector();

    expect(screen.getByRole("group", { name: "Hear it as" })).toBeInTheDocument();
    expect(screen.getByRole("button", { name: "Straightforward" })).toHaveAttribute("aria-pressed", "true");
    expect(screen.getByRole("button", { name: "Analytics Nerd" })).toHaveAttribute("aria-pressed", "false");
    expect(screen.getByRole("button", { name: "Gen Z" })).toBeInTheDocument();
  });

  it("switches vibe when a chip is chosen", () => {
    const { setMode } = renderSelector();

    fireEvent.click(screen.getByRole("button", { name: "Gen Z" }));
    expect(setMode).toHaveBeenCalledWith("gen_z");
  });

  it("surprise me picks a vibe other than the current one", () => {
    const { setMode } = renderSelector({ mode: "gen_z" });

    fireEvent.click(screen.getByRole("button", { name: /surprise me/i }));
    expect(setMode).toHaveBeenCalledTimes(1);
    expect(["straightforward", "analytics_dork"]).toContain(setMode.mock.calls[0][0]);
  });

  it("renders nothing until vibes have loaded", () => {
    renderSelector({ vibes: [] });
    expect(screen.queryByRole("group", { name: "Hear it as" })).not.toBeInTheDocument();
  });
});

describe("pickSurpriseVibe", () => {
  const ids = ["a", "b", "c", "d"];

  it("never returns the current vibe", () => {
    for (const random of [0, 0.3, 0.6, 0.999]) {
      expect(pickSurpriseVibe(ids, "b", () => random)).not.toBe("b");
    }
  });

  it("can reach every other vibe", () => {
    const picks = new Set([0, 0.4, 0.8].map((random) => pickSurpriseVibe(ids, "a", () => random)));
    expect(picks).toEqual(new Set(["b", "c", "d"]));
  });

  it("falls back to the only vibe, and to null when there are none", () => {
    expect(pickSurpriseVibe(["a"], "a")).toBe("a");
    expect(pickSurpriseVibe([], "a")).toBeNull();
  });
});
