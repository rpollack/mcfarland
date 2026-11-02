import Anthropic from "@anthropic-ai/sdk";
import type { AnalysisMode } from "./vibes.js";

interface AnalysisResponse {
  prompt: string;
  persona: string;
  headline: string;
  analysis: string;
  cached: boolean;
}

interface FantasyDecisionResponse {
  prompt: string;
  decision: "START" | "SIT";
  confidence: "Low" | "Medium" | "High";
  headline: string;
  analysis: string;
  cached: boolean;
}

type ParsedAnalysisPayload = {
  headline: string;
  analysis: string;
};

type ParsedFantasyPayload = {
  decision: "START" | "SIT";
  confidence: "Low" | "Medium" | "High";
  headline: string;
  analysis: string;
};

type ChatHandler = (prompt: string, persona: string, mode: AnalysisMode) => Promise<AnalysisResponse>;
type FantasyDecisionHandler = (prompt: string) => Promise<FantasyDecisionResponse>;

let testChatHandler: ChatHandler | null = null;
let testFantasyDecisionHandler: FantasyDecisionHandler | null = null;
const uncacheableResponses = new WeakSet<AnalysisResponse>();
const uncacheableFantasyResponses = new WeakSet<FantasyDecisionResponse>();
const DEFAULT_ANTHROPIC_MODEL = "claude-sonnet-5-5";
/** Headline used when no analysis could be produced (missing API key or a failed model call). */
export const ANALYSIS_UNAVAILABLE_HEADLINE = "Analysis unavailable";

export function buildStructuredAnalysisPrompt(prompt: string, persona: string, mode: AnalysisMode): string {
  const modeSpecificInstructions =
    mode === "shakespeare"
      ? [
          "Mode-specific requirement for Shakespeare:",
          "- The analysis should read like stage verse, not ordinary prose.",
          "- Use unrhymed lines that aim for iambic pentameter whenever possible.",
          "- Do not mention meter or explain the gimmick; just write in it.",
          "- Keep the statistical claims precise and grounded even while the language is heightened.",
          "",
        ]
      : mode === "gen_z"
        ? [
            "Mode-specific requirement for Gen Z:",
            "- Keep the Gen Z voice active across the full analysis, not just the opening sentence or first paragraph.",
            "- Let slang, internet cadence, and emoji-level energy show up throughout the piece.",
            "- Do not abruptly snap back into plain default analyst prose midway through the response.",
            "- Keep the stats accurate, but the whole read should still sound like the same over-the-top fan voice from start to finish.",
            "",
          ]
      : [];

  return [
    "Return one raw JSON object with a full baseball analysis and a headline based on it.",
    "- Return raw JSON only. Do not wrap it in markdown fences.",
    `- Analysis vibe: ${persona}`,
    "- Make the chosen vibe meaningfully alter word choice, rhythm, and framing throughout; do not flatten back into neutral analyst prose.",
    "- Stay statistically grounded and within the prompt.",
    "",
    ...modeSpecificInstructions,
    "JSON shape:",
    '{"headline":"...","analysis":"..."}',
    "",
    "Write the analysis first, following the prompt exactly:",
    "",
    prompt,
    "",
    "For headline:",
    "- Write one concise, specific, non-clickbait headline from the finished analysis.",
    "- Use words over stat notation; avoid metric lists, extra numbers, first person, puns, and overstatement.",
    "- Return headline text only inside the headline field.",
    "",
    "For analysis:",
    "- Put the full analysis in the analysis field only.",
    "- Format it as 3-5 short paragraphs separated by blank lines.",
    "- Do not repeat the headline verbatim as the first sentence.",
    "- Do not add extra keys, labels, or commentary.",
  ].join("\n");
}

function coerceParsedAnalysisPayload(parsed: unknown): ParsedAnalysisPayload | null {
  if (typeof parsed === "string") {
    return parseJsonPayload(parsed);
  }

  if (!parsed || typeof parsed !== "object") {
    return null;
  }

  const payload = parsed as Partial<ParsedAnalysisPayload>;
  if (typeof payload.headline !== "string" || typeof payload.analysis !== "string") {
    return null;
  }

  return {
    headline: payload.headline.trim(),
    analysis: payload.analysis.trim(),
  };
}

function unescapeJsonString(value: string): string {
  try {
    return JSON.parse(`"${value.replace(/"/g, '\\"')}"`) as string;
  } catch {
    return value.replace(/\\"/g, '"').replace(/\\n/g, "\n").replace(/\\\\/g, "\\");
  }
}

function parseLooseJsonPayload(candidate: string): ParsedAnalysisPayload | null {
  const headlineMatch = candidate.match(/"headline"\s*:\s*"((?:\\.|[^"\\])*)"/);
  const analysisMatch = candidate.match(/"analysis"\s*:\s*"([\s\S]*)"\s*\}?$/);

  if (!headlineMatch || !analysisMatch) {
    return null;
  }

  const headline = unescapeJsonString(headlineMatch[1]).trim();
  const analysis = unescapeJsonString(analysisMatch[1]).trim();

  return headline && analysis ? { headline, analysis } : null;
}

export function parseJsonPayload(text: string): ParsedAnalysisPayload | null {
  const trimmed = text.trim();
  const direct = trimmed.match(/^\{[\s\S]*\}$/);
  const fenced = trimmed.match(/```json\s*([\s\S]*?)```/i)?.[1] ?? trimmed.match(/```([\s\S]*?)```/)?.[1];
  const candidate = direct?.[0] ?? fenced ?? trimmed;

  if (!candidate) {
    return null;
  }

  try {
    return coerceParsedAnalysisPayload(JSON.parse(candidate));
  } catch {
    return parseLooseJsonPayload(candidate);
  }
}

function parseFantasyJsonPayload(text: string): ParsedFantasyPayload | null {
  const trimmed = text.trim();
  const direct = trimmed.match(/^\{[\s\S]*\}$/);
  const candidate = direct?.[0] ?? trimmed.match(/```json\s*([\s\S]*?)```/i)?.[1] ?? trimmed.match(/```([\s\S]*?)```/)?.[1];

  if (!candidate) {
    return null;
  }

  try {
    const parsed = JSON.parse(candidate) as Partial<ParsedFantasyPayload>;
    const decision = parsed.decision === "START" || parsed.decision === "SIT" ? parsed.decision : null;
    const confidence = parsed.confidence === "Low" || parsed.confidence === "Medium" || parsed.confidence === "High" ? parsed.confidence : null;
    if (!decision || !confidence || typeof parsed.headline !== "string" || typeof parsed.analysis !== "string") {
      return null;
    }
    return {
      decision,
      confidence,
      headline: parsed.headline.trim(),
      analysis: parsed.analysis.trim(),
    };
  } catch {
    return null;
  }
}

function buildFallbackHeadline(analysis: string): string {
  const normalized = analysis
    .replace(/^#+\s*/gm, "")
    .replace(/\*\*/g, "")
    .trim();
  const firstLine = normalized.split(/\n+/).find((line) => line.trim().length > 0)?.trim() ?? "";
  const firstSentence = firstLine.split(/(?<=[.!?])\s+/)[0]?.trim() ?? "";
  const candidate = (firstSentence || firstLine).replace(/[.!?]+$/, "").trim();
  return candidate || "Analysis available";
}

const SYSTEM_PROMPT =
  "You are an expert in advanced baseball statistics and in translating them for casual fans. Use only the stats provided in the prompt. Do not invent injuries, mechanics, lineup role, or other outside context. Let sample size affect your confidence, and do not let stylistic persona instructions override factual accuracy or statistical rigor. Write like a perceptive human baseball analyst, not like a template or an automated report.";

const ANALYSIS_OUTPUT_SCHEMA = {
  type: "object",
  properties: {
    headline: { type: "string" },
    analysis: { type: "string" },
  },
  required: ["headline", "analysis"],
  additionalProperties: false,
};

const FANTASY_OUTPUT_SCHEMA = {
  type: "object",
  properties: {
    decision: { type: "string", enum: ["START", "SIT"] },
    confidence: { type: "string", enum: ["Low", "Medium", "High"] },
    headline: { type: "string" },
    analysis: { type: "string" },
  },
  required: ["decision", "confidence", "headline", "analysis"],
  additionalProperties: false,
};

async function runMessage(apiKey: string, prompt: string, schema: Record<string, unknown>): Promise<string> {
  const model = process.env.ANTHROPIC_MODEL?.trim() || DEFAULT_ANTHROPIC_MODEL;
  // Haiku has no server-side refusal fallback; other current models retry declines on Anthropic's recommended model.
  const fallbackOptions = model.startsWith("claude-haiku")
    ? {}
    : { betas: ["server-side-fallback-2026-07-01"], fallbacks: "default" as const };
  const response = await new Anthropic({ apiKey }).beta.messages.create({
    model,
    max_tokens: 16000,
    ...fallbackOptions,
    output_config: { effort: "medium", format: { type: "json_schema", schema } },
    system: SYSTEM_PROMPT,
    messages: [{ role: "user", content: prompt }],
  });

  if (response.stop_reason === "refusal") {
    throw new Error(`Request declined${response.stop_details?.category ? ` (${response.stop_details.category})` : ""}`);
  }

  const text = response.content
    .filter((block): block is Anthropic.Beta.BetaTextBlock => block.type === "text")
    .map((block) => block.text)
    .join("")
    .trim();

  return text || "No response returned from the Anthropic API.";
}

export async function callClaudeAnalysis(prompt: string, persona: string, mode: AnalysisMode): Promise<AnalysisResponse> {
  if (testChatHandler) {
    return testChatHandler(prompt, persona, mode);
  }

  const apiKey = process.env.ANTHROPIC_API_KEY;
  if (!apiKey) {
    const response = {
      prompt,
      persona,
      headline: ANALYSIS_UNAVAILABLE_HEADLINE,
      analysis: "Anthropic API key is not configured. Provide an ANTHROPIC_API_KEY environment variable to enable AI analysis.",
      cached: false,
    };
    uncacheableResponses.add(response);
    return response;
  }

  try {
    const payload = await runMessage(apiKey, buildStructuredAnalysisPrompt(prompt, persona, mode), ANALYSIS_OUTPUT_SCHEMA);
    const parsed = parseJsonPayload(payload);
    const analysis = parsed?.analysis || payload;
    const headline = parsed?.headline || buildFallbackHeadline(analysis);
    const response = { prompt, persona, headline, analysis, cached: false };

    if (!parsed) {
      uncacheableResponses.add(response);
    }

    return response;
  } catch (error) {
    const response = {
      prompt,
      persona,
      headline: ANALYSIS_UNAVAILABLE_HEADLINE,
      analysis: `Anthropic API error: ${error instanceof Error ? error.message : "Unknown error"}`,
      cached: false,
    };
    uncacheableResponses.add(response);
    return response;
  }
}

export async function callClaudeFantasyDecision(prompt: string): Promise<FantasyDecisionResponse> {
  if (testFantasyDecisionHandler) {
    return testFantasyDecisionHandler(prompt);
  }

  const apiKey = process.env.ANTHROPIC_API_KEY;
  if (!apiKey) {
    const response = {
      prompt,
      decision: "SIT" as const,
      confidence: "Low" as const,
      headline: "Fantasy decision unavailable",
      analysis: "Anthropic API key is not configured. Provide an ANTHROPIC_API_KEY environment variable to enable fantasy matchup decisions.",
      cached: false,
    };
    uncacheableFantasyResponses.add(response);
    return response;
  }

  try {
    const payload = await runMessage(
      apiKey,
      [
        "Return raw JSON only. Do not wrap the response in markdown fences.",
        "The JSON must match exactly this shape:",
        '{"decision":"START|SIT","confidence":"Low|Medium|High","headline":"...","analysis":"..."}',
        "",
        prompt,
      ].join("\n"),
      FANTASY_OUTPUT_SCHEMA
    );
    const parsed = parseFantasyJsonPayload(payload);
    if (!parsed) {
      const response = {
        prompt,
        decision: "SIT" as const,
        confidence: "Low" as const,
        headline: "Fantasy decision unavailable",
        analysis: payload,
        cached: false,
      };
      uncacheableFantasyResponses.add(response);
      return response;
    }

    return { prompt, ...parsed, cached: false };
  } catch (error) {
    const response = {
      prompt,
      decision: "SIT" as const,
      confidence: "Low" as const,
      headline: "Fantasy decision unavailable",
      analysis: `Anthropic API error: ${error instanceof Error ? error.message : "Unknown error"}`,
      cached: false,
    };
    uncacheableFantasyResponses.add(response);
    return response;
  }
}

export function isCacheableAnalysisResponse(response: AnalysisResponse): boolean {
  return (
    !uncacheableResponses.has(response) &&
    response.headline !== ANALYSIS_UNAVAILABLE_HEADLINE &&
    !response.analysis.startsWith("Anthropic API error:")
  );
}

export function isCacheableFantasyDecision(response: FantasyDecisionResponse): boolean {
  return (
    !uncacheableFantasyResponses.has(response) &&
    response.headline !== "Fantasy decision unavailable" &&
    !response.analysis.startsWith("Anthropic API error:")
  );
}

export function __setAnalysisHandlerForTests(handler: ChatHandler | null): void {
  testChatHandler = handler;
}

export function __setFantasyDecisionHandlerForTests(handler: FantasyDecisionHandler | null): void {
  testFantasyDecisionHandler = handler;
}
