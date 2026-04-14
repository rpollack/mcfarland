import crypto from "crypto";
import type { AnalysisMode } from "./vibes.js";

const cache = new Map<string, { analysis: string; headline: string; persona: string; timestamp: number }>();
const CACHE_TTL_MS = 1000 * 60 * 60; // 1 hour

interface OpenAIResponse {
  prompt: string;
  persona: string;
  headline: string;
  analysis: string;
  cached: boolean;
}

type ParsedAnalysisPayload = {
  headline: string;
  analysis: string;
};

function buildStructuredAnalysisPrompt(prompt: string): string {
  return [
    prompt,
    "",
    "Return valid JSON only with this exact shape:",
    '{"headline":"...","analysis":"..."}',
    "",
    "For headline:",
    "- Be concise.",
    "- Be specific and grounded in the analysis.",
    "- Use words, not stat notation.",
    "- Do not include parenthetical metric lists or multiple embedded numbers unless absolutely necessary.",
    "- Do not turn the headline into a mini-analysis.",
    "- Focus on the single most important tension or takeaway.",
    "- Do not use clickbait or exaggeration.",
    "- Do not use first-person language.",
    "- Prefer substance over puns.",
    "- Reflect the actual confidence level of the analysis and do not overstate small-sample conclusions.",
    "",
    "For analysis:",
    "- Put the full analysis in the analysis field only.",
    "- Do not repeat the headline verbatim as the first sentence.",
  ].join("\n");
}

function parseJsonPayload(text: string): ParsedAnalysisPayload | null {
  const trimmed = text.trim();
  const direct = trimmed.match(/^\{[\s\S]*\}$/);
  const candidate = direct?.[0] ?? trimmed.match(/```json\s*([\s\S]*?)```/i)?.[1] ?? trimmed.match(/```([\s\S]*?)```/)?.[1];

  if (!candidate) {
    return null;
  }

  try {
    const parsed = JSON.parse(candidate) as Partial<ParsedAnalysisPayload>;
    if (typeof parsed.headline !== "string" || typeof parsed.analysis !== "string") {
      return null;
    }
    return {
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

async function runChatCompletion(apiKey: string, prompt: string): Promise<string> {
  const response = await fetch("https://api.openai.com/v1/chat/completions", {
    method: "POST",
    headers: {
      Authorization: `Bearer ${apiKey}`,
      "Content-Type": "application/json",
    },
    body: JSON.stringify({
      model: "gpt-5-mini",
      messages: [
        {
          role: "system",
          content:
            "You are an expert in advanced baseball statistics and in translating them for casual fans. Use only the stats provided in the prompt. Do not invent injuries, mechanics, lineup role, or other outside context. Let sample size affect your confidence, and do not let stylistic persona instructions override factual accuracy or statistical rigor. Write like a perceptive human baseball analyst, not like a template or an automated report.",
        },
        { role: "user", content: prompt },
      ],
    }),
  });

  if (!response.ok) {
    const error = await response.json().catch(() => ({ error: { message: response.statusText } }));
    throw new Error(error?.error?.message ?? response.statusText);
  }

  const payload = (await response.json()) as {
    choices?: { message?: { content?: string } }[];
  };

  return payload.choices?.[0]?.message?.content?.trim() || "No response returned from the OpenAI API.";
}

export async function callOpenAiChat(prompt: string, persona: string, mode: AnalysisMode): Promise<OpenAIResponse> {
  const cacheKey = crypto.createHash("sha256").update(`${mode}:${prompt}`).digest("hex");
  const cached = cache.get(cacheKey);
  if (cached && Date.now() - cached.timestamp < CACHE_TTL_MS) {
    return { prompt, persona, headline: cached.headline, analysis: cached.analysis, cached: true };
  }

  const apiKey = process.env.OPENAI_API_KEY;
  if (!apiKey) {
    return {
      prompt,
      persona,
      headline: "Analysis unavailable",
      analysis: "OpenAI API key is not configured. Provide an OPENAI_API_KEY environment variable to enable AI analysis.",
      cached: false,
    };
  }

  try {
    const payload = await runChatCompletion(apiKey, buildStructuredAnalysisPrompt(prompt));
    const parsed = parseJsonPayload(payload);
    const analysis = parsed?.analysis || payload;
    const headline = parsed?.headline || buildFallbackHeadline(analysis);

    cache.set(cacheKey, { analysis, headline, persona, timestamp: Date.now() });

    return { prompt, persona, headline, analysis, cached: false };
  } catch (error) {
    return {
      prompt,
      persona,
      headline: "Analysis unavailable",
      analysis: `OpenAI API error: ${error instanceof Error ? error.message : "Unknown error"}`,
      cached: false,
    };
  }
}
