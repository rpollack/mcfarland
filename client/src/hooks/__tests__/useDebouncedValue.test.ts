import { act, renderHook } from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { useDebouncedValue } from "../useDebouncedValue";

describe("useDebouncedValue", () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });

  afterEach(() => {
    vi.useRealTimers();
  });

  it("returns the initial value immediately and only follows changes after the delay", () => {
    const { result, rerender } = renderHook(({ value }) => useDebouncedValue(value, 175), {
      initialProps: { value: "ju" },
    });
    expect(result.current).toBe("ju");

    rerender({ value: "jud" });
    act(() => {
      vi.advanceTimersByTime(174);
    });
    expect(result.current).toBe("ju");

    act(() => {
      vi.advanceTimersByTime(1);
    });
    expect(result.current).toBe("jud");
  });

  it("restarts the delay on every change so only the last value lands", () => {
    const { result, rerender } = renderHook(({ value }) => useDebouncedValue(value, 175), {
      initialProps: { value: "" },
    });

    rerender({ value: "ju" });
    act(() => {
      vi.advanceTimersByTime(100);
    });
    rerender({ value: "judge" });
    act(() => {
      vi.advanceTimersByTime(100);
    });
    expect(result.current).toBe("");

    act(() => {
      vi.advanceTimersByTime(75);
    });
    expect(result.current).toBe("judge");
  });
});
