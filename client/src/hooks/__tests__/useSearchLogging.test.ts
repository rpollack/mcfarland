import { renderHook } from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import * as api from "../../api";
import { toLoggedQuery, useSearchLogging } from "../useSearchLogging";

type Props = { term: string; resultCount: number | undefined; playerType?: "hitter" | "pitcher" };

describe("useSearchLogging", () => {
  let logSpy: ReturnType<typeof vi.spyOn>;

  beforeEach(() => {
    logSpy = vi.spyOn(api, "logUiEvent").mockResolvedValue();
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  function setup(initialProps: Props) {
    return renderHook(
      ({ term, resultCount, playerType = "hitter" }: Props) =>
        useSearchLogging({ surface: "single", playerType, term, resultCount }),
      { initialProps }
    );
  }

  it("waits for results before logging", () => {
    setup({ term: "judge", resultCount: undefined });

    expect(logSpy).not.toHaveBeenCalled();
  });

  it("logs a settled search once, including its result count", () => {
    const { rerender } = setup({ term: "judge", resultCount: undefined });

    rerender({ term: "judge", resultCount: 3 });
    rerender({ term: "judge", resultCount: 3 });

    expect(logSpy).toHaveBeenCalledTimes(1);
    expect(logSpy).toHaveBeenCalledWith({
      event: "player_search_performed",
      surface: "single",
      playerType: "hitter",
      query: "judge",
      resultCount: 3,
    });
  });

  it("logs searches that found nothing", () => {
    setup({ term: "zzzz", resultCount: 0 });

    expect(logSpy).toHaveBeenCalledWith(expect.objectContaining({ query: "zzzz", resultCount: 0 }));
  });

  it("logs again for a new term, a new player type, or the same term typed again", () => {
    const { rerender } = setup({ term: "ju", resultCount: 5 });

    rerender({ term: "judge", resultCount: 1 });
    rerender({ term: "judge", resultCount: 1, playerType: "pitcher" });
    expect(logSpy).toHaveBeenCalledTimes(3);

    rerender({ term: "", resultCount: undefined, playerType: "pitcher" });
    rerender({ term: "judge", resultCount: 1, playerType: "pitcher" });
    expect(logSpy).toHaveBeenCalledTimes(4);
  });
});

describe("toLoggedQuery", () => {
  it("trims and caps what is sent", () => {
    expect(toLoggedQuery("  judge ")).toBe("judge");
    expect(toLoggedQuery("a".repeat(200))).toHaveLength(64);
  });
});
