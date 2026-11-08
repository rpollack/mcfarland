import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { buildStructuredAnalysisPrompt, callClaudeAnalysis, callClaudeFantasyDecision, parseJsonPayload } from "../src/claude.js";
import { buildModelUsageRecord } from "../src/modelUsage.js";

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
  beforeEach(() => {
    // An empty key disables Amplitude forwarding, so fetch calls in these tests are only the model request.
    process.env.AMPLITUDE_API_KEY = "";
    vi.spyOn(console, "info").mockImplementation(() => undefined);
  });

  afterEach(() => {
    vi.restoreAllMocks();
    delete process.env.ANTHROPIC_API_KEY;
    delete process.env.ANTHROPIC_MODEL;
    delete process.env.AMPLITUDE_API_KEY;
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


describe("model usage logging", () => {
  const usage = {
    input_tokens: 1200,
    output_tokens: 900,
    output_tokens_details: { thinking_tokens: 300 },
    cache_creation_input_tokens: 10,
    cache_read_input_tokens: 20,
  };
  const goodBody = {
    content: [{ type: "text", text: JSON.stringify({ headline: "Headline", analysis: "Para one.\n\nPara two." }) }],
    stop_reason: "end_turn",
  };

  function loggedRecords(infoSpy: ReturnType<typeof vi.spyOn>) {
    return infoSpy.mock.calls
      .filter(([label]) => label === "[model-usage]")
      .map(([, json]) => JSON.parse(String(json)));
  }

  beforeEach(() => {
    process.env.ANTHROPIC_API_KEY = "test-key";
    process.env.AMPLITUDE_API_KEY = "";
  });

  afterEach(() => {
    vi.restoreAllMocks();
    delete process.env.ANTHROPIC_API_KEY;
    delete process.env.ANTHROPIC_MODEL;
    delete process.env.AMPLITUDE_API_KEY;
  });

  it("logs input, output, thinking and cache tokens with the model, effort, vibe and call kind", async () => {
    const infoSpy = vi.spyOn(console, "info").mockImplementation(() => undefined);
    mockAnthropicResponse({ ...goodBody, usage });

    await callClaudeAnalysis("Analyze Player X.", "Straightforward", "gen_z", "compare");

    const [record] = loggedRecords(infoSpy);
    expect(record).toMatchObject({
      kind: "compare",
      analysisMode: "gen_z",
      model: "claude-sonnet-5-5",
      servedModel: "claude-sonnet-5-5",
      fellBack: false,
      effort: "medium",
      stopReason: "end_turn",
      inputTokens: 1200,
      outputTokens: 900,
      thinkingTokens: 300,
      cacheCreationInputTokens: 10,
      cacheReadInputTokens: 20,
    });
    expect(typeof record.durationMs).toBe("number");
  });

  it("does not put the prompt or any player data in the log", async () => {
    const infoSpy = vi.spyOn(console, "info").mockImplementation(() => undefined);
    mockAnthropicResponse({ ...goodBody, usage });

    await callClaudeAnalysis("Analyze Aaron Judge, 285 PA.", "Straightforward", "straightforward");

    const logged = infoSpy.mock.calls.map((call) => call.join(" ")).join("\n");
    expect(logged).not.toContain("Judge");
    expect(logged).not.toContain("285");
  });

  it("defaults to the single-player kind and records fantasy calls as fantasy", async () => {
    const infoSpy = vi.spyOn(console, "info").mockImplementation(() => undefined);
    mockAnthropicResponse({ ...goodBody, usage });
    await callClaudeAnalysis("Analyze Player X.", "Straightforward", "straightforward");

    mockAnthropicResponse({
      content: [{ type: "text", text: JSON.stringify({ decision: "START", confidence: "High", headline: "H", analysis: "A" }) }],
      stop_reason: "end_turn",
      usage,
    });
    await callClaudeFantasyDecision("Start or sit?");

    const records = loggedRecords(infoSpy);
    expect(records.map((record) => record.kind)).toEqual(["single", "fantasy"]);
    expect(records[1].analysisMode).toBeNull();
  });

  it("flags when the refusal fallback model served the response", async () => {
    const infoSpy = vi.spyOn(console, "info").mockImplementation(() => undefined);
    mockAnthropicResponse({
      ...goodBody,
      model: "claude-fable-5-1",
      usage: {
        ...usage,
        iterations: [{ type: "fallback_message", model: "claude-fable-5-1", input_tokens: 1200, output_tokens: 900, cache_creation_input_tokens: 0, cache_read_input_tokens: 0 }],
      },
    });

    await callClaudeAnalysis("Analyze Player X.", "Straightforward", "straightforward");

    expect(loggedRecords(infoSpy)[0]).toMatchObject({ model: "claude-sonnet-5-5", servedModel: "claude-fable-5-1", fellBack: true });
  });

  it("still logs tokens when the request is declined", async () => {
    const infoSpy = vi.spyOn(console, "info").mockImplementation(() => undefined);
    mockAnthropicResponse({ content: [], stop_reason: "refusal", usage });

    const result = await callClaudeAnalysis("Analyze Player X.", "Straightforward", "straightforward");

    expect(result.headline).toBe("Analysis unavailable");
    expect(loggedRecords(infoSpy)[0]).toMatchObject({ stopReason: "refusal", inputTokens: 1200, outputTokens: 900 });
  });

  it("does not log when no model call is made", async () => {
    delete process.env.ANTHROPIC_API_KEY;
    const infoSpy = vi.spyOn(console, "info").mockImplementation(() => undefined);

    await callClaudeAnalysis("Analyze Player X.", "Straightforward", "straightforward");

    expect(loggedRecords(infoSpy)).toEqual([]);
  });

  it("sends a model_call_completed event to Amplitude with snake_case properties and no user data", async () => {
    process.env.AMPLITUDE_API_KEY = "amp-test-key";
    vi.spyOn(console, "info").mockImplementation(() => undefined);
    const fetchSpy = mockAnthropicResponse({ ...goodBody, usage });

    await callClaudeAnalysis("Analyze Player X.", "Straightforward", "old_coot", "mirage_intro");

    await vi.waitFor(() => expect(fetchSpy).toHaveBeenCalledTimes(2));
    const [url, init] = fetchSpy.mock.calls[1] as [string, RequestInit];
    expect(String(url)).toContain("api2.amplitude.com");
    const event = JSON.parse(String(init.body)).events[0];
    expect(event.event_type).toBe("model_call_completed");
    expect(event.user_id).toBe("server");
    expect(event.event_properties).toMatchObject({
      kind: "mirage_intro",
      analysis_mode: "old_coot",
      model: "claude-sonnet-5-5",
      served_model: "claude-sonnet-5-5",
      fell_back: false,
      effort: "medium",
      input_tokens: 1200,
      output_tokens: 900,
      thinking_tokens: 300,
      cache_creation_input_tokens: 10,
      cache_read_input_tokens: 20,
    });
    expect(JSON.stringify(event)).not.toContain("Analyze Player X");
  });

  it("returns null for token counts the API did not report", () => {
    const record = buildModelUsageRecord(
      { model: "claude-sonnet-5-5", stop_reason: "end_turn", usage: { input_tokens: 50, output_tokens: 20 } },
      { kind: "single", model: "claude-sonnet-5-5", effort: "medium", durationMs: 5 }
    );
    expect(record).toMatchObject({ inputTokens: 50, outputTokens: 20, thinkingTokens: null, cacheCreationInputTokens: null, cacheReadInputTokens: null, fellBack: false });

    const empty = buildModelUsageRecord(
      { model: "claude-sonnet-5-5", stop_reason: null },
      { kind: "single", model: "claude-sonnet-5-5", effort: "medium", durationMs: 5 }
    );
    expect(empty).toMatchObject({ inputTokens: null, outputTokens: null, thinkingTokens: null, stopReason: null });
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
