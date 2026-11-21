import request from "supertest";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

const analytics = vi.hoisted(() => ({
  logSpendLimitHit: vi.fn(async () => undefined),
  logSpendDailySummary: vi.fn(async () => undefined),
}));
vi.mock("../src/analytics.js", async (importOriginal) => ({ ...(await importOriginal<typeof import("../src/analytics.js")>()), ...analytics }));

import { createServer } from "../src/index.js";
import { __setAnalysisCacheStoreForTests } from "../src/analysisCache.js";
import { __setAnalysisHandlerForTests, __setFantasyDecisionHandlerForTests } from "../src/claude.js";
import { __clearMlbMatchupCacheForTests } from "../src/mlbMatchups.js";
import type { ModelUsageRecord } from "../src/modelUsage.js";
import { __resetSpendGuardForTests, __setSpendStoreForTests, checkBudget, getSpendReport, MemorySpendStore, recordSpend, SpendLimitError, type SpendStore } from "../src/spendGuard.js";

const app = createServer();

// $4 on claude-sonnet-5-5 output: 400,000 output tokens at $10/M.
function usageRecord(outputTokens: number, kind: ModelUsageRecord["kind"] = "single"): ModelUsageRecord {
  return {
    kind,
    analysisMode: null,
    model: "claude-sonnet-5-5",
    servedModel: "claude-sonnet-5-5",
    fellBack: false,
    effort: "medium",
    stopReason: "end_turn",
    inputTokens: 0,
    outputTokens,
    thinkingTokens: null,
    cacheCreationInputTokens: 0,
    cacheReadInputTokens: 0,
    durationMs: 1,
  };
}

const check = (overrides: Partial<Parameters<typeof checkBudget>[0]> = {}) =>
  checkBudget({ kind: "single", ip: "1.1.1.1", isAdmin: false, sessionId: "sess-1", ...overrides });

describe("spend guard", () => {
  beforeEach(() => {
    __resetSpendGuardForTests();
    __setSpendStoreForTests(new MemorySpendStore());
    analytics.logSpendLimitHit.mockClear();
    analytics.logSpendDailySummary.mockClear();
    vi.spyOn(console, "warn").mockImplementation(() => undefined);
    vi.spyOn(console, "info").mockImplementation(() => undefined);
    vi.stubEnv("MODEL_MONTHLY_BUDGET_USD", "50");
    vi.stubEnv("MODEL_CAP_ENFORCE", "true");
  });

  afterEach(() => {
    vi.useRealTimers();
    vi.unstubAllEnvs();
    vi.restoreAllMocks();
    __resetSpendGuardForTests();
  });

  it("is off when no monthly budget is set", async () => {
    vi.stubEnv("MODEL_MONTHLY_BUDGET_USD", "");
    await recordSpend(usageRecord(100_000_000));
    await expect(check()).resolves.toBeUndefined();
  });

  it("allows requests under budget", async () => {
    await recordSpend(usageRecord(400_000)); // $4 of a $5 daily ceiling
    await expect(check()).resolves.toBeUndefined();
  });

  it("blocks when today's spend reaches the daily ceiling (10% of the monthly budget by default)", async () => {
    await recordSpend(usageRecord(500_000)); // $5
    const failure = await check().catch((error) => error);
    expect(failure).toBeInstanceOf(SpendLimitError);
    expect(failure.limitType).toBe("daily");
    expect(failure.message).toContain("today's analysis limit");
  });

  it("honors MODEL_DAILY_CEILING_USD", async () => {
    vi.stubEnv("MODEL_DAILY_CEILING_USD", "1");
    await recordSpend(usageRecord(100_000)); // $1
    await expect(check()).rejects.toMatchObject({ limitType: "daily" });
  });

  it("blocks when the month total reaches the budget, with the monthly wording", async () => {
    vi.stubEnv("MODEL_DAILY_CEILING_USD", "100");
    await recordSpend(usageRecord(5_000_000)); // $50
    const failure = await check().catch((error) => error);
    expect(failure).toBeInstanceOf(SpendLimitError);
    expect(failure.limitType).toBe("monthly");
    expect(failure.message).toContain("this month's analysis limit");
  });

  it("rolls unused days over: a new UTC day under the month budget is allowed again", async () => {
    vi.useFakeTimers({ toFake: ["Date"] });
    vi.setSystemTime(new Date("2026-11-10T12:00:00Z"));
    await recordSpend(usageRecord(500_000)); // $5, hits the daily ceiling
    await expect(check()).rejects.toBeInstanceOf(SpendLimitError);

    vi.setSystemTime(new Date("2026-11-11T00:00:01Z"));
    await expect(check()).resolves.toBeUndefined();
  });

  it("starts a new month with a clean month total", async () => {
    vi.stubEnv("MODEL_DAILY_CEILING_USD", "100");
    vi.useFakeTimers({ toFake: ["Date"] });
    vi.setSystemTime(new Date("2026-11-30T12:00:00Z"));
    await recordSpend(usageRecord(5_000_000));
    await expect(check()).rejects.toMatchObject({ limitType: "monthly" });

    vi.setSystemTime(new Date("2026-12-01T00:00:01Z"));
    await expect(check()).resolves.toBeUndefined();
  });

  it("caps cache-miss generations per IP per hour", async () => {
    vi.stubEnv("MODEL_IP_HOURLY_CAP", "2");
    vi.useFakeTimers({ toFake: ["Date"] });
    vi.setSystemTime(new Date("2026-11-10T12:00:00Z"));
    await check();
    await check();
    await expect(check()).rejects.toMatchObject({ limitType: "ip", retryAfterSeconds: 3600 });
    await expect(check({ ip: "2.2.2.2" })).resolves.toBeUndefined();

    vi.setSystemTime(new Date("2026-11-10T13:00:01Z"));
    await expect(check()).resolves.toBeUndefined();
  });

  it("turns the per-IP cap off with MODEL_IP_HOURLY_CAP=0", async () => {
    vi.stubEnv("MODEL_IP_HOURLY_CAP", "0");
    for (let i = 0; i < 50; i += 1) {
      await check();
    }
  });

  it("never blocks admins, but their spend counts and nothing is sent to Amplitude", async () => {
    vi.stubEnv("MODEL_IP_HOURLY_CAP", "1");
    await recordSpend(usageRecord(500_000));
    await expect(check({ isAdmin: true })).resolves.toBeUndefined();
    await expect(check({ isAdmin: true })).resolves.toBeUndefined();
    expect(analytics.logSpendLimitHit).not.toHaveBeenCalled();
    expect((await getSpendReport()).today.costUsd).toBeCloseTo(5, 6);
  });

  it("in log-only mode records the would-block but lets the request through", async () => {
    vi.stubEnv("MODEL_CAP_ENFORCE", "");
    await recordSpend(usageRecord(500_000));

    await expect(check()).resolves.toBeUndefined();

    expect(analytics.logSpendLimitHit).toHaveBeenCalledWith(
      expect.objectContaining({ kind: "single", limitType: "daily", enforced: false }),
      "sess-1"
    );
    expect(console.warn).toHaveBeenCalledWith("[spend-guard] limit hit", expect.objectContaining({ limitType: "daily", enforced: false }));
    const report = await getSpendReport();
    expect(report.enforce).toBe(false);
    expect(report.hitsLast7Days[0]).toMatchObject({ daily: 1, monthly: 0, ip: 0, distinctSessions: 1 });
  });

  it("stores hits without IP, prompt or player data", async () => {
    const store = new MemorySpendStore();
    const addHit = vi.spyOn(store, "addHit");
    __setSpendStoreForTests(store);
    await recordSpend(usageRecord(500_000));

    await expect(check({ ip: "9.9.9.9" })).rejects.toBeInstanceOf(SpendLimitError);

    const serialized = JSON.stringify([addHit.mock.calls, analytics.logSpendLimitHit.mock.calls]);
    expect(serialized).not.toContain("9.9.9.9");
    expect(Object.keys(addHit.mock.calls[0][0]).sort()).toEqual(
      ["daySpendUsd", "enforced", "hitAt", "kind", "limitType", "monthSpendUsd", "sessionId"]
    );
  });

  it("fails open on the in-process tally when storage throws", async () => {
    const broken: SpendStore = {
      addSpend: async () => { throw new Error("db down"); },
      totals: async () => { throw new Error("db down"); },
      addHit: async () => { throw new Error("db down"); },
      hitsSince: async () => { throw new Error("db down"); },
      dayStats: async () => { throw new Error("db down"); },
    };
    __setSpendStoreForTests(broken);

    await expect(check()).resolves.toBeUndefined();
    await recordSpend(usageRecord(500_000));
    // The tally is still enforced locally even though the database is down.
    await expect(check()).rejects.toMatchObject({ limitType: "daily" });
    expect((await getSpendReport()).today.costUsd).toBeCloseTo(5, 6);
  });

  it("warns about unavailable storage at most once an hour", async () => {
    __setSpendStoreForTests({
      addSpend: async () => { throw new Error("db down"); },
      totals: async () => { throw new Error("db down"); },
      addHit: async () => undefined,
      hitsSince: async () => [],
      dayStats: async () => ({ costUsd: 0, calls: 0, hits: 0, distinctSessions: 0 }),
    });
    vi.useFakeTimers({ toFake: ["Date"] });
    vi.setSystemTime(new Date("2026-11-10T12:00:00Z"));
    await recordSpend(usageRecord(1));
    await recordSpend(usageRecord(1));
    const spendWarnings = () => vi.mocked(console.warn).mock.calls.filter(([message]) => String(message).includes("using in-process tally")).length;
    expect(spendWarnings()).toBe(1);

    vi.setSystemTime(new Date("2026-11-10T13:00:01Z"));
    await recordSpend(usageRecord(1));
    expect(spendWarnings()).toBe(2);
  });

  it("emits one daily summary the first time it runs after UTC midnight", async () => {
    vi.useFakeTimers({ toFake: ["Date"] });
    vi.setSystemTime(new Date("2026-11-10T23:59:00Z"));
    await recordSpend(usageRecord(500_000));
    await expect(check()).rejects.toBeInstanceOf(SpendLimitError);

    vi.setSystemTime(new Date("2026-11-11T00:00:05Z"));
    await check();
    await check();
    await vi.waitFor(() => expect(analytics.logSpendDailySummary).toHaveBeenCalledTimes(1));
    expect(analytics.logSpendDailySummary).toHaveBeenCalledWith({ day: "2026-11-10", costUsd: 5, calls: 1, hits: 1, distinctSessionsBlocked: 1 });
  });

  it("caches totals briefly instead of reading storage on every request", async () => {
    const store = new MemorySpendStore();
    const totals = vi.spyOn(store, "totals");
    __setSpendStoreForTests(store);
    await check();
    await check();
    await check();
    expect(totals).toHaveBeenCalledTimes(1);
  });
});

describe("spend guard routes", () => {
  const players = async () => (await request(app).get("/api/players").query({ type: "hitter", q: "Judge" })).body.players;

  beforeEach(() => {
    __resetSpendGuardForTests();
    __setSpendStoreForTests(new MemorySpendStore());
    __setAnalysisCacheStoreForTests(null);
    __clearMlbMatchupCacheForTests();
    vi.spyOn(console, "warn").mockImplementation(() => undefined);
    vi.spyOn(console, "error").mockImplementation(() => undefined);
    vi.spyOn(console, "info").mockImplementation(() => undefined);
    vi.stubEnv("MODEL_MONTHLY_BUDGET_USD", "50");
    vi.stubEnv("MODEL_CAP_ENFORCE", "true");
  });

  afterEach(() => {
    delete process.env.ADMIN_PASSWORD;
    vi.unstubAllEnvs();
    vi.restoreAllMocks();
    __setAnalysisHandlerForTests(null);
    __setFantasyDecisionHandlerForTests(null);
    __setAnalysisCacheStoreForTests(null);
    __resetSpendGuardForTests();
  });

  async function exhaustBudget() {
    await recordSpend(usageRecord(500_000));
  }

  it("answers 429 with code, message and Retry-After when the limit blocks a cache miss", async () => {
    const handler = vi.fn(async () => ({ headline: "h", analysis: "a", prompt: "p", persona: "x", cached: false }));
    __setAnalysisHandlerForTests(handler as never);
    await exhaustBudget();
    const [player] = await players();

    const response = await request(app).post("/api/analyze").send({ playerId: player.id, playerType: "hitter", analysisMode: "gen_z" });

    expect(response.status).toBe(429);
    expect(response.body.code).toBe("spend_limit");
    expect(response.body.error).toContain("today's analysis limit");
    const retryAfter = Number(response.headers["retry-after"]);
    expect(retryAfter).toBeGreaterThan(0);
    expect(retryAfter).toBeLessThanOrEqual(86_400);
    expect(handler).not.toHaveBeenCalled();
  });

  it("guards compare and mirage-board intro generations too", async () => {
    await exhaustBudget();
    const found = await players();
    const compare = await request(app)
      .post("/api/compare/analyze")
      .send({ playerType: "hitter", playerIds: [found[0].id, found[1]?.id ?? found[0].id], analysisMode: "gen_z" });
    const mirage = await request(app).post("/api/mirage-board/intro").send({ playerType: "hitter", analysisMode: "gen_z" });

    expect(compare.status).toBe(429);
    expect(mirage.status).toBe(429);
  });

  it("serves a cached take even when the budget is spent, and never counts it", async () => {
    const calls: string[] = [];
    __setAnalysisHandlerForTests((async () => {
      calls.push("model");
      return { headline: "Cached take", analysis: "Body" };
    }) as never);
    const rows = new Map<string, { headline: string; analysis: string }>();
    __setAnalysisCacheStoreForTests({
      get: async ({ cacheKey }) => rows.get(cacheKey) ?? null,
      set: async (entry) => { rows.set(entry.cacheKey, { headline: entry.headline, analysis: entry.analysis }); },
      prune: async () => undefined,
    });
    const [player] = await players();
    const body = { playerId: player.id, playerType: "hitter", analysisMode: "gen_z" };

    const first = await request(app).post("/api/analyze").send(body);
    expect(first.status).toBe(200);
    await new Promise((resolve) => setImmediate(resolve));
    await exhaustBudget();
    const second = await request(app).post("/api/analyze").send(body);

    expect(second.status).toBe(200);
    expect(second.body.cached).toBe(true);
    expect(calls).toHaveLength(1);
    expect((await getSpendReport()).hitsLast7Days).toEqual([]);
  });

  it("lets requests through in log-only mode", async () => {
    vi.stubEnv("MODEL_CAP_ENFORCE", "");
    __setAnalysisHandlerForTests((async () => ({ headline: "h", analysis: "a" })) as never);
    await exhaustBudget();
    const [player] = await players();

    const response = await request(app).post("/api/analyze").send({ playerId: player.id, playerType: "hitter", analysisMode: "gen_z" });

    expect(response.status).toBe(200);
    expect((await getSpendReport()).hitsLast7Days[0].daily).toBe(1);
  });

  it("guards fantasy generations after their own cache lookup", async () => {
    await exhaustBudget();
    const handler = vi.fn();
    __setFantasyDecisionHandlerForTests(handler as never);
    const [player] = await players();
    vi.spyOn(globalThis, "fetch").mockResolvedValue({ ok: true, status: 200, statusText: "OK", json: async () => ({ dates: [], people: [] }) } as Response);

    const response = await request(app).post("/api/fantasy/daily-matchup/analyze").send({ playerId: player.id, playerType: "hitter" });

    expect(response.status).toBe(429);
    expect(response.body.code).toBe("spend_limit");
    expect(handler).not.toHaveBeenCalled();
  });

  describe("GET /api/admin/spend", () => {
    it("requires admin access", async () => {
      process.env.ADMIN_PASSWORD = "top-secret";
      const response = await request(app).get("/api/admin/spend");
      expect(response.status).toBe(403);
    });

    it("returns spend, limits and recent hits to an admin", async () => {
      process.env.ADMIN_PASSWORD = "top-secret";
      await exhaustBudget();
      await check().catch(() => undefined);

      const response = await request(app).get("/api/admin/spend?admin=top-secret");

      expect(response.status).toBe(200);
      expect(response.body).toMatchObject({
        today: { calls: 1 },
        month: { calls: 1 },
        budget: 50,
        ceiling: 5,
        enforce: true,
      });
      expect(response.body.today.costUsd).toBeCloseTo(5, 6);
      expect(response.body.hitsLast7Days[0]).toMatchObject({ daily: 1, monthly: 0, ip: 0, distinctSessions: 1 });
      expect(JSON.stringify(response.body)).not.toContain("top-secret");
    });
  });
});
