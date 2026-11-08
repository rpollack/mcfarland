import type Anthropic from "@anthropic-ai/sdk";
import { logModelCall } from "./analytics.js";

/** What the model call was for, so cost can be broken down by feature. */
export type ModelCallKind = "single" | "compare" | "mirage_intro" | "fantasy";

export type ModelUsageRecord = {
  kind: ModelCallKind;
  /** Analysis vibe, for calls that have one. */
  analysisMode: string | null;
  /** Model requested. */
  model: string;
  /** Model that served the response; differs from `model` when the refusal fallback ran. */
  servedModel: string | null;
  fellBack: boolean;
  effort: string;
  stopReason: string | null;
  inputTokens: number | null;
  /** Billed output tokens, including thinking tokens. */
  outputTokens: number | null;
  thinkingTokens: number | null;
  cacheCreationInputTokens: number | null;
  cacheReadInputTokens: number | null;
  durationMs: number;
};

export interface ModelUsageMeta {
  kind: ModelCallKind;
  analysisMode?: string | null;
  model: string;
  effort: string;
  durationMs: number;
}

function count(value: unknown): number | null {
  return typeof value === "number" && Number.isFinite(value) ? value : null;
}

/** Pulls the cost-relevant fields out of a model response, tolerating any missing usage data. */
export function buildModelUsageRecord(
  response: Pick<Anthropic.Beta.BetaMessage, "model" | "stop_reason"> & { usage?: Partial<Anthropic.Beta.BetaUsage> | null },
  meta: ModelUsageMeta
): ModelUsageRecord {
  const usage = response.usage ?? {};
  return {
    kind: meta.kind,
    analysisMode: meta.analysisMode ?? null,
    model: meta.model,
    servedModel: typeof response.model === "string" ? response.model : null,
    fellBack: Array.isArray(usage.iterations) && usage.iterations.some((iteration) => iteration.type === "fallback_message"),
    effort: meta.effort,
    stopReason: response.stop_reason ?? null,
    inputTokens: count(usage.input_tokens),
    outputTokens: count(usage.output_tokens),
    thinkingTokens: count(usage.output_tokens_details?.thinking_tokens),
    cacheCreationInputTokens: count(usage.cache_creation_input_tokens),
    cacheReadInputTokens: count(usage.cache_read_input_tokens),
    durationMs: meta.durationMs,
  };
}

/** Logs one model call. Never throws: usage logging must not affect the response. */
export function recordModelUsage(record: ModelUsageRecord): void {
  try {
    console.info("[model-usage]", JSON.stringify(record));
    void logModelCall(record);
  } catch (error) {
    console.warn("[model-usage] failed to record usage", error);
  }
}
