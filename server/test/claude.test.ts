import { afterEach, describe, expect, it, vi } from "vitest";
import { buildStructuredAnalysisPrompt, callClaudeAnalysis, parseJsonPayload } from "../src/claude.js";

function mockAnthropicResponse(body: Record<string, unknown>) {
  return vi.spyOn(globalThis, "fetch").mockImplementation(
    async () =>
      new Response(
        JSON.stringify({
          id: "msg_test",
          type: "message",
          role: "assistant",
          model: "claude-sonnet-5-5",
          stop_sequence: null,
          usage: { input_tokens: 1, output_tokens: 1 },
          ...body,
        }),
        { status: 200, headers: { "content-type": "application/json" } }
      )
  );
}

describe("Claude analysis request", () => {
  afterEach(() => {
    vi.restoreAllMocks();
    delete process.env.ANTHROPIC_API_KEY;
    delete process.env.ANTHROPIC_MODEL;
  });

  it("sends a structured-output request to Claude Sonnet 5.5 with refusal fallback and parses the result", async () => {
    process.env.ANTHROPIC_API_KEY = "test-key";
    const fetchSpy = mockAnthropicResponse({
      content: [{ type: "text", text: JSON.stringify({ headline: "Judge keeps mashing", analysis: "Para one.\n\nPara two." }) }],
      stop_reason: "end_turn",
    });

    const result = await callClaudeAnalysis("Analyze Player X.", "Straightforward", "straightforward");

    expect(result).toMatchObject({ headline: "Judge keeps mashing", analysis: "Para one.\n\nPara two.", cached: false });
    const [url, init] = fetchSpy.mock.calls[0] as [string, RequestInit];
    expect(String(url)).toContain("/v1/messages");
    const body = JSON.parse(String(init.body));
    expect(body.model).toBe("claude-sonnet-5-5");
    expect(body.fallbacks).toBe("default");
    expect(body.output_config.format.type).toBe("json_schema");
    expect(body.output_config.format.schema.required).toEqual(["headline", "analysis"]);
    expect(body.messages[0].content).toContain("Analyze Player X.");
  });

  it("honors ANTHROPIC_MODEL, skips fallback for Haiku, and reports refusals as uncacheable errors", async () => {
    process.env.ANTHROPIC_API_KEY = "test-key";
    process.env.ANTHROPIC_MODEL = "claude-haiku-5-5";
    const fetchSpy = mockAnthropicResponse({ content: [], stop_reason: "refusal" });

    const result = await callClaudeAnalysis("Analyze Player X.", "Straightforward", "straightforward");

    const body = JSON.parse(String((fetchSpy.mock.calls[0][1] as RequestInit).body));
    expect(body.model).toBe("claude-haiku-5-5");
    expect(body.fallbacks).toBeUndefined();
    expect(result.headline).toBe("Analysis unavailable");
    expect(result.analysis).toMatch(/^Anthropic API error: Request declined/);
  });
});


describe("structured Claude prompt", () => {
  it("requires vibes to meaningfully alter writing style", () => {
    const prompt = buildStructuredAnalysisPrompt("Analyze Player X.", "Write with tabloid sportswriter energy.", "sensationalist");

    expect(prompt).toContain("meaningfully alter word choice, rhythm, and framing");
    expect(prompt).toContain("do not flatten back into neutral analyst prose");
    expect(prompt).toContain("Stay statistically grounded");
    expect(prompt).toContain("3-5 short paragraphs separated by blank lines");
  });

  it("parses JSON returned as an escaped string", () => {
    const parsed = parseJsonPayload(
      JSON.stringify(JSON.stringify({
        headline: "Klein shows real gains",
        analysis: "The first paragraph lands the takeaway.\n\nThe second paragraph adds context.",
      }))
    );

    expect(parsed).toEqual({
      headline: "Klein shows real gains",
      analysis: "The first paragraph lands the takeaway.\n\nThe second paragraph adds context.",
    });
  });

  it("loosely parses JSON-like output with literal paragraph breaks", () => {
    const parsed = parseJsonPayload(
      '{"headline":"Klein shows real gains","analysis":"The first paragraph lands the takeaway.\n\nThe second paragraph adds context."}'
    );

    expect(parsed).toEqual({
      headline: "Klein shows real gains",
      analysis: "The first paragraph lands the takeaway.\n\nThe second paragraph adds context.",
    });
  });
});
