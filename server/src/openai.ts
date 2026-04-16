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

function buildStructuredAnalysisPrompt(prompt: string, persona: string, mode: AnalysisMode): string {
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
    "You are doing the same two tasks as the previous version of this feature, but you must return both results in one JSON object.",
    "",
    "Task 1: write the full baseball analysis from the prompt below.",
    "Task 2: then distill that analysis into a factual but compelling headline.",
    "",
    "Important:",
    "- Keep the analysis as tight, controlled, and statistically grounded as the original prompt implies.",
    "- Do not loosen the tone, broaden the scope, or improvise extra structure just because you are returning JSON.",
    "- The headline should feel like it was written after reading the completed analysis, not independently.",
    "- Return raw JSON only. Do not wrap it in markdown fences.",
    `- Make the analysis clearly reflect the requested vibe in tone and phrasing: ${persona}`,
    "- Preserve that vibe strongly enough that different modes produce noticeably different writing styles, while keeping the facts grounded in the stats.",
    "- Sustain the vibe through every paragraph and the closing verdict; do not let the style fade after the opening.",
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
    "- You are a newspaper editor whose job is to distill a detailed baseball analysis into a factual but compelling headline.",
    "- Using the completed analysis, write a single headline that captures the most important takeaway and makes the reader want to keep reading.",
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
    "- Return headline text only inside the headline field.",
    "",
    "For analysis:",
    "- Put the full analysis in the analysis field only.",
    "- Do not repeat the headline verbatim as the first sentence.",
    "- Do not add extra keys, labels, or commentary.",
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
    const payload = await runChatCompletion(apiKey, buildStructuredAnalysisPrompt(prompt, persona, mode));
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
