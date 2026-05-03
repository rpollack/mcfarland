import { describe, expect, it } from "vitest";
import { buildStructuredAnalysisPrompt, parseJsonPayload } from "../src/openai.js";

describe("structured OpenAI prompt", () => {
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
