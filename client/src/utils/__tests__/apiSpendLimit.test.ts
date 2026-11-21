import { afterEach, describe, expect, it, vi } from "vitest";
import { analyzePlayer, SpendLimitError } from "../../api";

function mockResponse(status: number, body: unknown) {
  vi.spyOn(globalThis, "fetch").mockResolvedValue({
    ok: false,
    status,
    statusText: "Error",
    json: async () => body,
  } as Response);
}

describe("analysis request errors", () => {
  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("surfaces the server's spend-limit message as a SpendLimitError", async () => {
    mockResponse(429, { error: "We've hit today's analysis limit.", code: "spend_limit" });

    const failure = await analyzePlayer("1", "hitter", "gen_z").catch((error) => error);

    expect(failure).toBeInstanceOf(SpendLimitError);
    expect(failure.message).toBe("We've hit today's analysis limit.");
  });

  it("keeps other failures, including plain 429s, as generic errors", async () => {
    mockResponse(429, { error: "Too many requests" });

    const failure = await analyzePlayer("1", "hitter", "gen_z").catch((error) => error);

    expect(failure).not.toBeInstanceOf(SpendLimitError);
    expect(failure.message).toBe("Too many requests");
  });
});
