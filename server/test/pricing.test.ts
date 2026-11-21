import { beforeEach, describe, expect, it, vi } from "vitest";
import { __resetPricingWarningsForTests, CONSERVATIVE_RATES, estimateCostUsd, getModelRates } from "../src/pricing.js";

const usage = { inputTokens: 1_000_000, outputTokens: 1_000_000, cacheCreationInputTokens: 1_000_000, cacheReadInputTokens: 1_000_000 };

describe("estimateCostUsd", () => {
  beforeEach(() => {
    __resetPricingWarningsForTests();
    vi.restoreAllMocks();
  });

  it("prices claude-sonnet-5-5 at $2 in, $10 out, $2.50 cache write, $0.10 cache read per million", () => {
    expect(estimateCostUsd({ model: "claude-sonnet-5-5", servedModel: "claude-sonnet-5-5", ...usage })).toBeCloseTo(2 + 10 + 2.5 + 0.1, 6);
  });

  it("prices fixed token counts", () => {
    const cost = estimateCostUsd({
      model: "claude-sonnet-5-5",
      servedModel: null,
      inputTokens: 3000,
      outputTokens: 2000,
      cacheCreationInputTokens: null,
      cacheReadInputTokens: 10_000,
    });
    expect(cost).toBeCloseTo(0.006 + 0.02 + 0.001, 6);
  });

  it("bills the model that served the response, e.g. after a refusal fallback", () => {
    const base = { inputTokens: 1_000_000, outputTokens: 0, cacheCreationInputTokens: 0, cacheReadInputTokens: 0 };
    expect(estimateCostUsd({ model: "claude-sonnet-5-5", servedModel: "claude-opus-5-5", ...base })).toBeCloseTo(4, 6);
  });

  it("prices Haiku and Opus ids the app can use", () => {
    expect(getModelRates("claude-haiku-5-5").output).toBe(0.5);
    expect(getModelRates("claude-opus-5-5").output).toBe(20);
  });

  it("does not read claude-sonnet-5-5-<suffix> as claude-sonnet-5", () => {
    expect(getModelRates("claude-sonnet-5-5-20260101").cacheRead).toBe(0.1);
  });

  it("uses the most expensive known rates for unknown models and warns once", () => {
    const warn = vi.spyOn(console, "warn").mockImplementation(() => undefined);
    const record = { model: "claude-future-9", servedModel: null, ...usage };

    expect(estimateCostUsd(record)).toBeCloseTo(
      CONSERVATIVE_RATES.input + CONSERVATIVE_RATES.output + CONSERVATIVE_RATES.cacheWrite + CONSERVATIVE_RATES.cacheRead,
      6
    );
    estimateCostUsd(record);
    expect(CONSERVATIVE_RATES.output).toBe(50);
    expect(warn).toHaveBeenCalledTimes(1);
  });

  it("treats missing token counts as zero", () => {
    expect(estimateCostUsd({ model: "claude-sonnet-5-5", servedModel: null, inputTokens: null, outputTokens: null, cacheCreationInputTokens: null, cacheReadInputTokens: null })).toBe(0);
  });
});
