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

function buildHeadlinePrompt(analysis: string): string {
  return [
    "You are a newspaper editor whose job is to distill a detailed baseball analysis into a factual but compelling headline.",
    "",
    "Using the analysis below, write a single headline that captures the most important takeaway and makes the reader want to keep reading.",
    "",
    "Requirements:",
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
    "- Return only the headline text.",
    "",
    "Analysis:",
    analysis,
  ].join("\n");
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
    const analysis = await runChatCompletion(apiKey, prompt);
    const headline = await runChatCompletion(apiKey, buildHeadlinePrompt(analysis));

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
