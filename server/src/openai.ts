import crypto from "crypto";
import type { AnalysisMode } from "./vibes.js";

const cache = new Map<string, { analysis: string; persona: string; timestamp: number }>();
const CACHE_TTL_MS = 1000 * 60 * 60; // 1 hour

interface OpenAIResponse {
  prompt: string;
  persona: string;
  analysis: string;
  cached: boolean;
}

export async function callOpenAiChat(prompt: string, persona: string, mode: AnalysisMode): Promise<OpenAIResponse> {
  const cacheKey = crypto.createHash("sha256").update(`${mode}:${prompt}`).digest("hex");
  const cached = cache.get(cacheKey);
  if (cached && Date.now() - cached.timestamp < CACHE_TTL_MS) {
    return { prompt, persona, analysis: cached.analysis, cached: true };
  }

  const apiKey = process.env.OPENAI_API_KEY;
  if (!apiKey) {
    return {
      prompt,
      persona,
      analysis: "OpenAI API key is not configured. Provide an OPENAI_API_KEY environment variable to enable AI analysis.",
      cached: false,
    };
  }

  const response = await fetch("https://api.openai.com/v1/chat/completions", {
    method: "POST",
    headers: {
      Authorization: `Bearer ${apiKey}`,
      "Content-Type": "application/json",
    },
    body: JSON.stringify({
      model: "gpt-4.1-mini",
      messages: [
        {
          role: "system",
          content:
            "You are an expert in not only advances baseball statistics, but also in translating these stats for casual fans and making them more accessible for the lay audience.",
        },
        { role: "user", content: prompt },
      ],
      temperature: 0.4,
    }),
  });

  if (!response.ok) {
    const error = await response.json().catch(() => ({ error: { message: response.statusText } }));
    return {
      prompt,
      persona,
      analysis: `OpenAI API error: ${error?.error?.message ?? response.statusText}`,
      cached: false,
    };
  }

  const payload = (await response.json()) as {
    choices?: { message?: { content?: string } }[];
  };

  const analysis = payload.choices?.[0]?.message?.content ?? "No response returned from the OpenAI API.";

  cache.set(cacheKey, { analysis, persona, timestamp: Date.now() });

  return { prompt, persona, analysis, cached: false };
}
