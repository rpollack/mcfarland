import type { ModelUsageRecord } from "./modelUsage.js";

/** USD per million tokens. */
export type ModelRates = {
  input: number;
  output: number;
  /** Writing to the prompt cache (5-minute TTL). */
  cacheWrite: number;
  cacheRead: number;
};

/**
 * Anthropic first-party API list prices as of 2026-11-03 (claude-api reference, "Current Models" table).
 * Input/output prices and the cache-read prices for Opus 5.5 ($0.20) and Sonnet 5.5 ($0.10) are published values.
 * Cache writes use the documented 1.25x-input multiplier and cache reads for the remaining models use the documented
 * 0.1x-input multiplier, because no separate figure is listed for them. Haiku 5.5 uses its <=100K-token prompt prices
 * ($0.10 / $0.50); this app's prompts are far below that. Update this table (and the date) when prices change.
 * This is an estimate from token counts; Anthropic's console is the source of truth for billing.
 */
const RATES: Record<string, ModelRates> = {
  "claude-fable-5-2": { input: 8, output: 40, cacheWrite: 10, cacheRead: 0.2 },
  "claude-mythos-5-2": { input: 8, output: 40, cacheWrite: 10, cacheRead: 0.2 },
  "claude-fable-5-1": { input: 10, output: 50, cacheWrite: 12.5, cacheRead: 1 },
  "claude-mythos-5-1": { input: 10, output: 50, cacheWrite: 12.5, cacheRead: 1 },
  "claude-fable-5": { input: 10, output: 50, cacheWrite: 12.5, cacheRead: 1 },
  "claude-opus-5-5": { input: 4, output: 20, cacheWrite: 5, cacheRead: 0.2 },
  "claude-opus-5": { input: 5, output: 25, cacheWrite: 6.25, cacheRead: 0.5 },
  "claude-opus-4-8": { input: 5, output: 25, cacheWrite: 6.25, cacheRead: 0.5 },
  "claude-opus-4-7": { input: 5, output: 25, cacheWrite: 6.25, cacheRead: 0.5 },
  "claude-opus-4-6": { input: 5, output: 25, cacheWrite: 6.25, cacheRead: 0.5 },
  "claude-sonnet-5-5": { input: 2, output: 10, cacheWrite: 2.5, cacheRead: 0.1 },
  "claude-sonnet-5": { input: 2, output: 10, cacheWrite: 2.5, cacheRead: 0.2 },
  "claude-sonnet-4-6": { input: 3, output: 15, cacheWrite: 3.75, cacheRead: 0.3 },
  "claude-haiku-5-5": { input: 0.1, output: 0.5, cacheWrite: 0.125, cacheRead: 0.01 },
  "claude-haiku-4-5": { input: 1, output: 5, cacheWrite: 1.25, cacheRead: 0.1 },
};

/** Unknown models are billed at the most expensive known rates so the budget errs on the safe side. */
export const CONSERVATIVE_RATES: ModelRates = Object.values(RATES).reduce((max, rates) =>
  rates.output > max.output ? rates : max
);

const warnedModels = new Set<string>();

export function getModelRates(model: string | null | undefined): ModelRates {
  const id = model?.trim() ?? "";
  if (RATES[id]) {
    return RATES[id];
  }
  // Tolerate date-suffixed ids; the longest matching prefix wins so "claude-sonnet-5-5-x" is not read as "claude-sonnet-5".
  const prefix = Object.keys(RATES)
    .filter((key) => id.startsWith(`${key}-`))
    .sort((a, b) => b.length - a.length)[0];
  if (prefix) {
    return RATES[prefix];
  }
  if (!warnedModels.has(id)) {
    warnedModels.add(id);
    console.warn("[pricing] unknown model; estimating cost at the most expensive known rates", { model: id });
  }
  return CONSERVATIVE_RATES;
}

type PricedUsage = Pick<
  ModelUsageRecord,
  "model" | "servedModel" | "inputTokens" | "outputTokens" | "cacheCreationInputTokens" | "cacheReadInputTokens"
>;

/** Estimated USD cost of one model call. Output tokens already include thinking tokens. */
export function estimateCostUsd(record: PricedUsage): number {
  const rates = getModelRates(record.servedModel ?? record.model);
  const million = 1_000_000;
  return (
    ((record.inputTokens ?? 0) * rates.input +
      (record.outputTokens ?? 0) * rates.output +
      (record.cacheCreationInputTokens ?? 0) * rates.cacheWrite +
      (record.cacheReadInputTokens ?? 0) * rates.cacheRead) /
    million
  );
}

export function __resetPricingWarningsForTests(): void {
  warnedModels.clear();
}
