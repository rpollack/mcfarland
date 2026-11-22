import { afterEach, describe, expect, it, vi } from "vitest";
import { logSessionStart, logUiEvent, resolveAmplitudeApiKey } from "../src/analytics.js";

describe("Amplitude reporting gate", () => {
  afterEach(() => {
    vi.restoreAllMocks();
    vi.unstubAllEnvs();
  });

  it("reports to the built-in project only from live production", () => {
    expect(resolveAmplitudeApiKey({ NODE_ENV: "production" })).toBeTruthy();
  });

  it.each([
    ["unit tests", { NODE_ENV: "test" }],
    ["local development", {}],
    ["local development with NODE_ENV=development", { NODE_ENV: "development" }],
    ["CI, including the e2e run", { NODE_ENV: "production", CI: "true" }],
    ["vitest", { NODE_ENV: "production", VITEST: "true" }],
    ["a Render pull request preview deploy", { NODE_ENV: "production", IS_PULL_REQUEST: "true" }],
  ])("sends nothing during %s", (_label, env) => {
    expect(resolveAmplitudeApiKey(env as NodeJS.ProcessEnv)).toBeNull();
  });

  it("lets an explicit key opt in, and an empty key opt out", () => {
    expect(resolveAmplitudeApiKey({ NODE_ENV: "test", AMPLITUDE_API_KEY: " my-key " })).toBe("my-key");
    expect(resolveAmplitudeApiKey({ NODE_ENV: "production", AMPLITUDE_API_KEY: "" })).toBeNull();
  });

  it("makes no network call for any event type under test", async () => {
    const fetchSpy = vi.spyOn(globalThis, "fetch").mockResolvedValue({ ok: true } as Response);

    await logSessionStart("s1");
    await logUiEvent({ sessionId: "s1", eventType: "player_search_performed", properties: { query: "judge" } });

    expect(fetchSpy).not.toHaveBeenCalled();
  });
});
