import request from "supertest";
import { describe, expect, it, beforeAll, beforeEach, afterEach, vi } from "vitest";
import { createServer } from "../src/index.js";
import { __setAnalysisCacheStoreForTests } from "../src/analysisCache.js";
import { getDataFreshness, __setDataFreshnessForTests } from "../src/dataStore.js";
import { __setOpenAiChatHandlerForTests } from "../src/openai.js";

const app = createServer();
const originalFreshness = getDataFreshness();

function createMemoryCacheStore() {
  const rows = new Map<string, { dataThroughDate: string; headline: string; analysis: string }>();
  return {
    rows,
    store: {
      async get({ cacheKey, dataThroughDate }: { cacheKey: string; dataThroughDate: string }) {
        const row = rows.get(cacheKey);
        if (!row || row.dataThroughDate !== dataThroughDate) {
          return null;
        }
        return { headline: row.headline, analysis: row.analysis };
      },
      async set(entry: { cacheKey: string; dataThroughDate: string; headline: string; analysis: string }) {
        rows.set(entry.cacheKey, {
          dataThroughDate: entry.dataThroughDate,
          headline: entry.headline,
          analysis: entry.analysis,
        });
      },
      async prune(dataThroughDate: string) {
        for (const [cacheKey, row] of rows.entries()) {
          if (row.dataThroughDate !== dataThroughDate) {
            rows.delete(cacheKey);
          }
        }
      },
    },
  };
}

describe("McFARLAND API", () => {
  beforeAll(() => {
    process.env.OPENAI_API_KEY = ""; // ensure offline tests do not call the API
  });

  beforeEach(() => {
    process.env.OPENAI_API_KEY = "";
    __setAnalysisCacheStoreForTests(null);
    __setOpenAiChatHandlerForTests(null);
    __setDataFreshnessForTests(originalFreshness);
    vi.restoreAllMocks();
  });

  afterEach(() => {
    __setAnalysisCacheStoreForTests(null);
    __setOpenAiChatHandlerForTests(null);
    __setDataFreshnessForTests(originalFreshness);
    vi.restoreAllMocks();
  });

  it("returns health status", async () => {
    const response = await request(app).get("/health");
    expect(response.status).toBe(200);
    expect(response.body).toEqual({ status: "ok" });
  });

  it("lists hitters with fuzzy search", async () => {
    const response = await request(app).get("/api/players").query({ type: "hitter", q: "Judge" });
    expect(response.status).toBe(200);
    expect(response.body.players.length).toBeGreaterThan(0);
    expect(response.body.players[0].type).toBe("hitter");
  });

  it("returns player details and quick insight", async () => {
    const { body: listBody } = await request(app).get("/api/players").query({ type: "pitcher", q: "Skubal" });
    const pitcher = listBody.players[0];
    const response = await request(app).get(`/api/players/${pitcher.id}`).query({ type: "pitcher" });
    expect(response.status).toBe(200);
    expect(response.body.player.Name).toContain("Skubal");
    expect(typeof response.body.quickInsight).toBe("string");
  });

  it("returns a baseball-facing data freshness label", async () => {
    const response = await request(app).get("/api/data-freshness");

    expect(response.status).toBe(200);
    expect(response.body.dataThroughDate).toMatch(/^\d{4}-\d{2}-\d{2}$/);
    expect(response.body.dataThroughLabel).toMatch(/[A-Za-z]+\s+\d{1,2}/);
  });

  it("builds analysis prompts without calling OpenAI when key missing", async () => {
    const { body: listBody } = await request(app).get("/api/players").query({ type: "hitter", q: "Judge" });
    const hitter = listBody.players[0];
    const response = await request(app)
      .post("/api/analyze")
      .send({ playerId: hitter.id, playerType: "hitter", analysisMode: "gen_z" });

    expect(response.status).toBe(200);
    expect(response.body.prompt).toContain("Player: ");
    expect(response.body.analysis).toContain("OpenAI API key is not configured");
  });

  it("caches identical single-player analysis across sessions", async () => {
    const memoryCache = createMemoryCacheStore();
    __setAnalysisCacheStoreForTests(memoryCache.store);
    const openAiHandler = vi.fn(async (prompt: string, persona: string) => ({
      prompt,
      persona,
      headline: "Cached Judge headline",
      analysis: "Cached Judge analysis",
      cached: false,
    }));
    __setOpenAiChatHandlerForTests(openAiHandler);

    const { body: listBody } = await request(app).get("/api/players").query({ type: "hitter", q: "Judge" });
    const hitter = listBody.players[0];
    const first = await request(app)
      .post("/api/analyze")
      .set("x-session-id", "user-a")
      .send({ playerId: hitter.id, playerType: "hitter", analysisMode: "gen_z" });
    const second = await request(app)
      .post("/api/analyze")
      .set("x-session-id", "user-b")
      .send({ playerId: hitter.id, playerType: "hitter", analysisMode: "gen_z" });

    expect(first.status).toBe(200);
    expect(first.body.cached).toBe(false);
    expect(second.status).toBe(200);
    expect(second.body.cached).toBe(true);
    expect(second.body.headline).toBe("Cached Judge headline");
    expect(openAiHandler).toHaveBeenCalledTimes(1);
  });

  it("misses the analysis cache when vibe changes", async () => {
    const memoryCache = createMemoryCacheStore();
    __setAnalysisCacheStoreForTests(memoryCache.store);
    const openAiHandler = vi.fn(async (prompt: string, persona: string) => ({
      prompt,
      persona,
      headline: "Fresh headline",
      analysis: "Fresh analysis",
      cached: false,
    }));
    __setOpenAiChatHandlerForTests(openAiHandler);

    const { body: listBody } = await request(app).get("/api/players").query({ type: "hitter", q: "Judge" });
    const hitter = listBody.players[0];
    await request(app).post("/api/analyze").send({ playerId: hitter.id, playerType: "hitter", analysisMode: "gen_z" });
    await request(app).post("/api/analyze").send({ playerId: hitter.id, playerType: "hitter", analysisMode: "shakespeare" });

    expect(openAiHandler).toHaveBeenCalledTimes(2);
  });

  it("misses the analysis cache when data freshness changes", async () => {
    const memoryCache = createMemoryCacheStore();
    __setAnalysisCacheStoreForTests(memoryCache.store);
    const openAiHandler = vi.fn(async (prompt: string, persona: string) => ({
      prompt,
      persona,
      headline: "Fresh headline",
      analysis: "Fresh analysis",
      cached: false,
    }));
    __setOpenAiChatHandlerForTests(openAiHandler);

    const { body: listBody } = await request(app).get("/api/players").query({ type: "hitter", q: "Judge" });
    const hitter = listBody.players[0];
    await request(app).post("/api/analyze").send({ playerId: hitter.id, playerType: "hitter", analysisMode: "gen_z" });
    __setDataFreshnessForTests({ dataThroughDate: "2099-04-22", dataThroughLabel: "April 22" });
    const second = await request(app).post("/api/analyze").send({ playerId: hitter.id, playerType: "hitter", analysisMode: "gen_z" });

    expect(second.body.cached).toBe(false);
    expect(openAiHandler).toHaveBeenCalledTimes(2);
  });

  it("caches ordered comparison analysis separately from single-player analysis", async () => {
    const memoryCache = createMemoryCacheStore();
    __setAnalysisCacheStoreForTests(memoryCache.store);
    const openAiHandler = vi.fn(async (prompt: string, persona: string) => ({
      prompt,
      persona,
      headline: "Comparison headline",
      analysis: "Comparison analysis",
      cached: false,
    }));
    __setOpenAiChatHandlerForTests(openAiHandler);

    const { body: listBody } = await request(app).get("/api/players").query({ type: "hitter" });
    const playerIds = listBody.players.slice(0, 2).map((p: any) => p.id);
    const first = await request(app)
      .post("/api/compare/analyze")
      .send({ playerType: "hitter", playerIds, analysisMode: "straightforward" });
    const second = await request(app)
      .post("/api/compare/analyze")
      .send({ playerType: "hitter", playerIds, analysisMode: "straightforward" });

    expect(first.status).toBe(200);
    expect(first.body.cached).toBe(false);
    expect(second.status).toBe(200);
    expect(second.body.cached).toBe(true);
    expect(second.body.players).toHaveLength(2);
    expect(openAiHandler).toHaveBeenCalledTimes(1);
  });

  it("does not cache unavailable OpenAI responses", async () => {
    const memoryCache = createMemoryCacheStore();
    __setAnalysisCacheStoreForTests(memoryCache.store);
    const openAiHandler = vi.fn(async (prompt: string, persona: string) => ({
      prompt,
      persona,
      headline: "Analysis unavailable",
      analysis: "OpenAI API error: temporarily unavailable",
      cached: false,
    }));
    __setOpenAiChatHandlerForTests(openAiHandler);

    const { body: listBody } = await request(app).get("/api/players").query({ type: "hitter", q: "Judge" });
    const hitter = listBody.players[0];
    await request(app).post("/api/analyze").send({ playerId: hitter.id, playerType: "hitter", analysisMode: "gen_z" });
    await request(app).post("/api/analyze").send({ playerId: hitter.id, playerType: "hitter", analysisMode: "gen_z" });

    expect(openAiHandler).toHaveBeenCalledTimes(2);
    expect(memoryCache.rows.size).toBe(0);
  });

  it("does not cache malformed OpenAI responses", async () => {
    process.env.OPENAI_API_KEY = "test-key";
    const memoryCache = createMemoryCacheStore();
    __setAnalysisCacheStoreForTests(memoryCache.store);
    const fetchSpy = vi.spyOn(globalThis, "fetch").mockResolvedValue({
      ok: true,
      json: async () => ({ choices: [{ message: { content: "not json" } }] }),
    } as Response);

    const { body: listBody } = await request(app).get("/api/players").query({ type: "hitter", q: "Judge" });
    const hitter = listBody.players[0];
    await request(app).post("/api/analyze").send({ playerId: hitter.id, playerType: "hitter", analysisMode: "gen_z" });
    await request(app).post("/api/analyze").send({ playerId: hitter.id, playerType: "hitter", analysisMode: "gen_z" });

    expect(fetchSpy).toHaveBeenCalledTimes(2);
    expect(memoryCache.rows.size).toBe(0);
  });

  it("falls back to OpenAI when cache reads fail", async () => {
    __setAnalysisCacheStoreForTests({
      async get() {
        throw new Error("read failed");
      },
      async set() {},
      async prune() {},
    });
    const openAiHandler = vi.fn(async (prompt: string, persona: string) => ({
      prompt,
      persona,
      headline: "Fresh headline",
      analysis: "Fresh analysis",
      cached: false,
    }));
    __setOpenAiChatHandlerForTests(openAiHandler);

    const { body: listBody } = await request(app).get("/api/players").query({ type: "hitter", q: "Judge" });
    const hitter = listBody.players[0];
    const response = await request(app)
      .post("/api/analyze")
      .send({ playerId: hitter.id, playerType: "hitter", analysisMode: "gen_z" });

    expect(response.status).toBe(200);
    expect(response.body.analysis).toBe("Fresh analysis");
    expect(openAiHandler).toHaveBeenCalledTimes(1);
  });

  it("returns generated analysis when cache writes fail", async () => {
    __setAnalysisCacheStoreForTests({
      async get() {
        return null;
      },
      async set() {
        throw new Error("write failed");
      },
      async prune() {},
    });
    const openAiHandler = vi.fn(async (prompt: string, persona: string) => ({
      prompt,
      persona,
      headline: "Fresh headline",
      analysis: "Fresh analysis",
      cached: false,
    }));
    __setOpenAiChatHandlerForTests(openAiHandler);

    const { body: listBody } = await request(app).get("/api/players").query({ type: "hitter", q: "Judge" });
    const hitter = listBody.players[0];
    const response = await request(app)
      .post("/api/analyze")
      .send({ playerId: hitter.id, playerType: "hitter", analysisMode: "gen_z" });

    expect(response.status).toBe(200);
    expect(response.body.analysis).toBe("Fresh analysis");
  });

  it("logs Amplitude events for cache misses and hits", async () => {
    const memoryCache = createMemoryCacheStore();
    __setAnalysisCacheStoreForTests(memoryCache.store);
    const fetchSpy = vi.spyOn(globalThis, "fetch").mockResolvedValue({ ok: true } as Response);
    __setOpenAiChatHandlerForTests(async (prompt: string, persona: string) => ({
      prompt,
      persona,
      headline: "Tracked headline",
      analysis: "Tracked analysis",
      cached: false,
    }));

    const { body: listBody } = await request(app).get("/api/players").query({ type: "hitter", q: "Judge" });
    const hitter = listBody.players[0];
    await request(app)
      .post("/api/analyze")
      .set("x-session-id", "tracked-a")
      .send({ playerId: hitter.id, playerType: "hitter", analysisMode: "gen_z" });
    await request(app)
      .post("/api/analyze")
      .set("x-session-id", "tracked-b")
      .send({ playerId: hitter.id, playerType: "hitter", analysisMode: "gen_z" });

    await vi.waitFor(() => expect(fetchSpy).toHaveBeenCalledTimes(2));
  });

  it("prunes stale analysis cache rows after successful writes", async () => {
    const memoryCache = createMemoryCacheStore();
    memoryCache.rows.set("old-cache-key", {
      dataThroughDate: "2001-04-01",
      headline: "Old headline",
      analysis: "Old analysis",
    });
    __setAnalysisCacheStoreForTests(memoryCache.store);
    __setOpenAiChatHandlerForTests(async (prompt: string, persona: string) => ({
      prompt,
      persona,
      headline: "Fresh headline",
      analysis: "Fresh analysis",
      cached: false,
    }));

    const { body: listBody } = await request(app).get("/api/players").query({ type: "hitter", q: "Judge" });
    const hitter = listBody.players[0];
    await request(app).post("/api/analyze").send({ playerId: hitter.id, playerType: "hitter", analysisMode: "gen_z" });

    expect(memoryCache.rows.has("old-cache-key")).toBe(false);
  });

  it("recommends a comparison winner", async () => {
    const { body: listBody } = await request(app).get("/api/players").query({ type: "hitter" });
    const players = listBody.players.slice(0, 2);
    const response = await request(app)
      .post("/api/compare")
      .send({ playerType: "hitter", playerIds: players.map((p: any) => p.id) });

    expect(response.status).toBe(200);
    expect(response.body.players).toHaveLength(2);
    expect(response.body.recommendedPlayerId).toBeTypeOf("string");
  });

  it("returns persona metadata", async () => {
    const response = await request(app).get("/api/vibes");
    expect(response.status).toBe(200);
    expect(response.body.vibes).toHaveLength(9);
    expect(response.body.defaultMode).toBe("straightforward");
  });

  it("requires admin access for social suggestions when admin password is configured", async () => {
    process.env.ADMIN_PASSWORD = "top-secret";

    const response = await request(app).get("/api/admin/social-suggestions");

    expect(response.status).toBe(403);
    expect(response.body.error).toContain("Admin access required");
    delete process.env.ADMIN_PASSWORD;
  });

  it("returns fallback social suggestions when admin password is not configured", async () => {
    delete process.env.ADMIN_PASSWORD;

    const response = await request(app).get("/api/admin/social-suggestions");

    expect(response.status).toBe(200);
    expect(response.body.candidates).toHaveLength(3);
    expect(response.body.recommended.playerId).toBeTypeOf("string");
    expect(response.body.xPosts).toHaveLength(2);
    expect(response.body.blueskyPosts).toHaveLength(2);
  });

  it("returns trending quick links for hitters and pitchers", async () => {
    const response = await request(app).get("/api/trending");

    expect(response.status).toBe(200);
    expect(response.body.hitters.trending).toHaveLength(3);
    expect(response.body.hitters.breakouts).toHaveLength(3);
    expect(response.body.pitchers.trending).toHaveLength(3);
    expect(response.body.pitchers.breakouts).toHaveLength(3);
    expect(response.body.hitters.trending[0].type).toBe("hitter");
    expect(response.body.hitters.breakouts[0].type).toBe("hitter");
    expect(response.body.pitchers.trending[0].type).toBe("pitcher");
    expect(response.body.pitchers.breakouts[0].type).toBe("pitcher");
  });

  it("accepts share analytics events", async () => {
    const response = await request(app)
      .post("/api/share-events")
      .send({
        sessionId: "test-session",
        playerName: "Test Player",
        analysisMode: "default",
        eventType: "share_click",
      });

    expect(response.status).toBe(204);
  });

  it("returns weekly hitter and pitcher risers/fallers", async () => {
    const response = await request(app).get("/api/trends/weekly");

    expect(response.status).toBe(200);
    expect(response.body.weekStart).toMatch(/^\d{4}-\d{2}-\d{2}$/);
    expect(response.body.hitters?.risers).toHaveLength(3);
    expect(response.body.hitters?.fallers).toHaveLength(3);
    expect(response.body.pitchers?.risers).toHaveLength(3);
    expect(response.body.pitchers?.fallers).toHaveLength(3);
    expect(response.body.hitters.risers[0].type).toBe("hitter");
    expect(response.body.pitchers.risers[0].type).toBe("pitcher");
  });

  it("rejects trend refresh when secret is not configured", async () => {
    delete process.env.TRENDS_REFRESH_SECRET;

    const response = await request(app).post("/api/trends/refresh");

    expect(response.status).toBe(503);
    expect(response.body.error).toContain("not configured");
  });

  it("rejects trend refresh with the wrong secret", async () => {
    process.env.TRENDS_REFRESH_SECRET = "expected-secret";

    const response = await request(app)
      .post("/api/trends/refresh")
      .set("x-trends-refresh-secret", "wrong-secret");

    expect(response.status).toBe(403);
  });

  it("returns storage unavailable for trend refresh when database is not configured", async () => {
    process.env.TRENDS_REFRESH_SECRET = "expected-secret";
    delete process.env.DATABASE_URL;

    const response = await request(app)
      .post("/api/trends/refresh")
      .set("x-trends-refresh-secret", "expected-secret");

    expect(response.status).toBe(503);
    expect(response.body.error).toContain("storage is unavailable");
  });

  it("serves dynamic social preview HTML for share links", async () => {
    const { body: listBody } = await request(app).get("/api/players").query({ type: "hitter", q: "Judge" });
    const hitter = listBody.players[0];

    const response = await request(app)
      .get("/share")
      .query({ mode: "single", playerType: "hitter", playerId: hitter.id, vibe: "gen_z" });

    expect(response.status).toBe(200);
    expect(response.headers["content-type"]).toContain("text/html");
    expect(response.text).toContain("og:title");
    expect(response.text).toContain("twitter:title");
    expect(response.text).toContain("Judge");
    expect(response.text).toContain("/?mode=single");
  });

  it("uses a cached generated headline in share social preview titles", async () => {
    const memoryCache = createMemoryCacheStore();
    __setAnalysisCacheStoreForTests(memoryCache.store);
    __setOpenAiChatHandlerForTests(async (prompt: string, persona: string) => ({
      prompt,
      persona,
      headline: "Judge keeps forcing the issue",
      analysis: "Generated analysis body",
      cached: false,
    }));
    const { body: listBody } = await request(app).get("/api/players").query({ type: "hitter", q: "Judge" });
    const hitter = listBody.players[0];

    await request(app)
      .post("/api/analyze")
      .send({ playerId: hitter.id, playerType: "hitter", analysisMode: "gen_z" });
    const response = await request(app)
      .get("/share")
      .query({ mode: "single", playerType: "hitter", playerId: hitter.id, vibe: "gen_z" });

    expect(response.status).toBe(200);
    expect(response.text).toContain('property="og:title" content="Judge keeps forcing the issue | McFARLAND"');
    expect(response.text).toContain('name="twitter:title" content="Judge keeps forcing the issue | McFARLAND"');
  });

  it("injects OG tags into SPA HTML responses", async () => {
    const { body: listBody } = await request(app).get("/api/players").query({ type: "hitter" });
    const players = listBody.players.slice(0, 2);
    const response = await request(app)
      .get("/")
      .query({ mode: "compare", playerType: "hitter", playerIds: players.map((p: any) => p.id).join(",") });

    expect(response.status).toBe(200);
    expect(response.headers["content-type"]).toContain("text/html");
    expect(response.text).toContain("og:title");
    expect(response.text).toContain("twitter:image");
    expect(response.text).toContain("Comparison");
  });
});
