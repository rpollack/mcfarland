import request from "supertest";
import { describe, expect, it, beforeAll, beforeEach, afterEach, vi } from "vitest";
import { createServer } from "../src/index.js";
import { __setAnalysisCacheStoreForTests } from "../src/analysisCache.js";
import { getDataFreshness, getPlayerById, __setDataFreshnessForTests } from "../src/dataStore.js";
import { __clearMlbMatchupCacheForTests } from "../src/mlbMatchups.js";
import { __setFantasyDecisionHandlerForTests, __setAnalysisHandlerForTests } from "../src/claude.js";
import { formatEra, formatPercentage, formatStatValue } from "../src/utils.js";

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

// Cache writes are fire-and-forget, so let them finish before asserting on (or relying on) cache state.
async function settleCacheWrites() {
  await new Promise((resolve) => setImmediate(resolve));
}

function jsonResponse(body: unknown): Response {
  return {
    ok: true,
    status: 200,
    statusText: "OK",
    json: async () => body,
  } as Response;
}

function mockMlbApi({
  playerMlbamId,
  teamId = 147,
  opponentTeamId = 117,
  selectedProbablePitcherId,
  status = "Scheduled",
  battingOrder = [playerMlbamId],
  includeOpposingPitcher = true,
}: {
  playerMlbamId: number;
  teamId?: number;
  opponentTeamId?: number;
  selectedProbablePitcherId?: number;
  status?: string;
  battingOrder?: number[];
  includeOpposingPitcher?: boolean;
}) {
  const opposingPitcher = includeOpposingPitcher ? { id: 677960, fullName: "Ryan Weathers" } : undefined;
  const selectedStarter = selectedProbablePitcherId ? { id: selectedProbablePitcherId, fullName: "Selected Starter" } : { id: 681347, fullName: "Mike Burrows" };
  return vi.spyOn(globalThis, "fetch").mockImplementation(async (input: string | URL | Request) => {
    const url = String(input);
    if (url.includes("/api/v1/schedule")) {
      return jsonResponse({
        dates: [
          {
            games: [
              {
                gamePk: 824203,
                gameDate: "2026-04-25T23:10:00Z",
                status: { detailedState: status, statusCode: status === "Postponed" ? "DR" : "S" },
                teams: {
                  away: {
                    team: { id: teamId, name: "New York Yankees", abbreviation: "NYY" },
                    probablePitcher: selectedStarter,
                  },
                  home: {
                    team: { id: opponentTeamId, name: "Houston Astros", abbreviation: "HOU" },
                    probablePitcher: opposingPitcher,
                  },
                },
                venue: { name: "Daikin Park" },
                weather: { condition: "Clear", temp: "72", wind: "5 mph, Out To LF" },
              },
            ],
          },
        ],
      });
    }
    if (url.includes("/api/v1/people")) {
      return jsonResponse({
        people: [
          {
            id: playerMlbamId,
            fullName: "Selected Player",
            currentTeam: { id: teamId, name: "New York Yankees" },
            batSide: { code: "R" },
            pitchHand: { code: "R" },
          },
          { id: 677960, fullName: "Ryan Weathers", pitchHand: { code: "L" } },
          { id: 681347, fullName: "Mike Burrows", pitchHand: { code: "R" } },
          ...(selectedProbablePitcherId && selectedProbablePitcherId !== playerMlbamId
            ? [{ id: selectedProbablePitcherId, fullName: "Selected Starter", pitchHand: { code: "L" } }]
            : []),
        ],
      });
    }
    if (url.includes("/api/v1.1/game/824203/feed/live")) {
      return jsonResponse({
        liveData: {
          boxscore: {
            teams: {
              away: { battingOrder },
              home: { battingOrder: [] },
            },
          },
        },
      });
    }
    return jsonResponse({});
  });
}

describe("McFARLAND API", () => {
  beforeAll(() => {
    process.env.ANTHROPIC_API_KEY = ""; // ensure offline tests do not call the API
  });

  beforeEach(() => {
    process.env.ANTHROPIC_API_KEY = "";
    __setAnalysisCacheStoreForTests(null);
    __setAnalysisHandlerForTests(null);
    __setFantasyDecisionHandlerForTests(null);
    __clearMlbMatchupCacheForTests();
    __setDataFreshnessForTests(originalFreshness);
    vi.restoreAllMocks();
  });

  afterEach(() => {
    delete process.env.ADMIN_PASSWORD;
    __setAnalysisCacheStoreForTests(null);
    __setAnalysisHandlerForTests(null);
    __setFantasyDecisionHandlerForTests(null);
    __clearMlbMatchupCacheForTests();
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

  it("builds analysis prompts without calling Claude when key missing", async () => {
    const { body: listBody } = await request(app).get("/api/players").query({ type: "hitter", q: "Judge" });
    const hitter = listBody.players[0];
    const response = await request(app)
      .post("/api/analyze")
      .send({ playerId: hitter.id, playerType: "hitter", analysisMode: "gen_z" });

    expect(response.status).toBe(200);
    expect(response.body.prompt).toContain("Player: ");
    expect(response.body.analysis).toContain("Anthropic API key is not configured");
  });

  it("sends the selected hitter's loaded stats to the analysis prompt", async () => {
    const analysisHandler = vi.fn(async (prompt: string, persona: string) => ({
      prompt,
      persona,
      headline: "Hitter data headline",
      analysis: "Hitter data analysis",
      cached: false,
    }));
    __setAnalysisHandlerForTests(analysisHandler);

    const { body: listBody } = await request(app).get("/api/players").query({ type: "hitter", q: "Judge" });
    const hitter = listBody.players[0];
    const loadedPlayer = getPlayerById("hitter", hitter.id);

    expect(loadedPlayer).not.toBeNull();

    const response = await request(app)
      .post("/api/analyze")
      .send({ playerId: hitter.id, playerType: "hitter", analysisMode: "straightforward" });

    expect(response.status).toBe(200);
    expect(analysisHandler).toHaveBeenCalledTimes(1);

    const prompt = analysisHandler.mock.calls[0][0];
    const player = loadedPlayer!;
    expect(prompt).toContain(`Player: ${player.Name} (Hitter)`);
    expect(prompt).toContain(
      `AVG: ${formatStatValue(player.AVG_cur)} | WB ${formatStatValue(player.AVG_l3)} | diff ${formatStatValue(player.AVG_diff)} | diff from league-wide weighted baseline ${formatStatValue(player.AVG_lg_adj_diff)}`
    );
    expect(prompt).toContain(
      `wOBA: ${formatStatValue(player.wOBA_cur)} | WB ${formatStatValue(player.wOBA_l3)} | diff ${formatStatValue(player.wOBA_diff)} | diff from league-wide weighted baseline ${formatStatValue(player.wOBA_lg_adj_diff)}`
    );
    expect(prompt).toContain(
      `K%: ${formatPercentage(player.K_pct_cur)} | WB ${formatPercentage(player.K_pct_l3)} | diff ${formatPercentage(player.K_pct_diff)} | diff from league-wide weighted baseline ${formatPercentage(player.K_pct_lg_adj_diff)}`
    );
  });

  it("sends the selected pitcher's loaded stats to the analysis prompt", async () => {
    const analysisHandler = vi.fn(async (prompt: string, persona: string) => ({
      prompt,
      persona,
      headline: "Pitcher data headline",
      analysis: "Pitcher data analysis",
      cached: false,
    }));
    __setAnalysisHandlerForTests(analysisHandler);

    const { body: listBody } = await request(app).get("/api/players").query({ type: "pitcher", q: "Skubal" });
    const pitcher = listBody.players[0];
    const loadedPlayer = getPlayerById("pitcher", pitcher.id);

    expect(loadedPlayer).not.toBeNull();

    const response = await request(app)
      .post("/api/analyze")
      .send({ playerId: pitcher.id, playerType: "pitcher", analysisMode: "straightforward" });

    expect(response.status).toBe(200);
    expect(analysisHandler).toHaveBeenCalledTimes(1);

    const prompt = analysisHandler.mock.calls[0][0];
    const player = loadedPlayer!;
    expect(prompt).toContain(`Player: ${player.Name} (Pitcher)`);
    expect(prompt).toContain(
      `ERA: ${formatEra(player.era_cur)} | WB ${formatEra(player.era_l3)} | diff ${formatEra(player.era_diff)} | diff from league-wide weighted baseline ${formatEra(player.era_lg_adj_diff)}`
    );
    expect(prompt).toContain(
      `xERA: ${formatEra(player.xera_cur)} | WB ${formatEra(player.xera_l3)} | diff ${formatEra(player.xera_diff)} | diff from league-wide weighted baseline ${formatEra(player.xera_lg_adj_diff)}`
    );
    expect(prompt).toContain(
      `K-BB%: ${formatPercentage(player.k_minus_bb_percent_cur)} | WB ${formatPercentage(player.k_minus_bb_percent_l3)} | diff ${formatPercentage(player.k_minus_bb_percent_diff)} | diff from league-wide weighted baseline ${formatPercentage(player.k_minus_bb_percent_lg_adj_diff)}`
    );
  });

  it("caches identical single-player analysis across sessions", async () => {
    const memoryCache = createMemoryCacheStore();
    __setAnalysisCacheStoreForTests(memoryCache.store);
    const analysisHandler = vi.fn(async (prompt: string, persona: string) => ({
      prompt,
      persona,
      headline: "Cached Judge headline",
      analysis: "Cached Judge analysis",
      cached: false,
    }));
    __setAnalysisHandlerForTests(analysisHandler);

    const { body: listBody } = await request(app).get("/api/players").query({ type: "hitter", q: "Judge" });
    const hitter = listBody.players[0];
    const first = await request(app)
      .post("/api/analyze")
      .set("x-session-id", "user-a")
      .send({ playerId: hitter.id, playerType: "hitter", analysisMode: "gen_z" });
    await settleCacheWrites();
    const second = await request(app)
      .post("/api/analyze")
      .set("x-session-id", "user-b")
      .send({ playerId: hitter.id, playerType: "hitter", analysisMode: "gen_z" });

    expect(first.status).toBe(200);
    expect(first.body.cached).toBe(false);
    expect(second.status).toBe(200);
    expect(second.body.cached).toBe(true);
    expect(second.body.headline).toBe("Cached Judge headline");
    expect(analysisHandler).toHaveBeenCalledTimes(1);
  });

  it("misses the analysis cache when vibe changes", async () => {
    const memoryCache = createMemoryCacheStore();
    __setAnalysisCacheStoreForTests(memoryCache.store);
    const analysisHandler = vi.fn(async (prompt: string, persona: string) => ({
      prompt,
      persona,
      headline: "Fresh headline",
      analysis: "Fresh analysis",
      cached: false,
    }));
    __setAnalysisHandlerForTests(analysisHandler);

    const { body: listBody } = await request(app).get("/api/players").query({ type: "hitter", q: "Judge" });
    const hitter = listBody.players[0];
    await request(app).post("/api/analyze").send({ playerId: hitter.id, playerType: "hitter", analysisMode: "gen_z" });
    await request(app).post("/api/analyze").send({ playerId: hitter.id, playerType: "hitter", analysisMode: "shakespeare" });

    expect(analysisHandler).toHaveBeenCalledTimes(2);
  });

  it("misses the analysis cache when data freshness changes", async () => {
    const memoryCache = createMemoryCacheStore();
    __setAnalysisCacheStoreForTests(memoryCache.store);
    const analysisHandler = vi.fn(async (prompt: string, persona: string) => ({
      prompt,
      persona,
      headline: "Fresh headline",
      analysis: "Fresh analysis",
      cached: false,
    }));
    __setAnalysisHandlerForTests(analysisHandler);

    const { body: listBody } = await request(app).get("/api/players").query({ type: "hitter", q: "Judge" });
    const hitter = listBody.players[0];
    await request(app).post("/api/analyze").send({ playerId: hitter.id, playerType: "hitter", analysisMode: "gen_z" });
    await settleCacheWrites();
    __setDataFreshnessForTests({ dataThroughDate: "2099-04-22", dataThroughLabel: "April 22" });
    const second = await request(app).post("/api/analyze").send({ playerId: hitter.id, playerType: "hitter", analysisMode: "gen_z" });

    expect(second.body.cached).toBe(false);
    expect(analysisHandler).toHaveBeenCalledTimes(2);
  });

  it("caches ordered comparison analysis separately from single-player analysis", async () => {
    const memoryCache = createMemoryCacheStore();
    __setAnalysisCacheStoreForTests(memoryCache.store);
    const analysisHandler = vi.fn(async (prompt: string, persona: string) => ({
      prompt,
      persona,
      headline: "Comparison headline",
      analysis: "Comparison analysis",
      cached: false,
    }));
    __setAnalysisHandlerForTests(analysisHandler);

    const { body: listBody } = await request(app).get("/api/players").query({ type: "hitter" });
    const playerIds = listBody.players.slice(0, 2).map((p: any) => p.id);
    const first = await request(app)
      .post("/api/compare/analyze")
      .send({ playerType: "hitter", playerIds, analysisMode: "straightforward" });
    await settleCacheWrites();
    const second = await request(app)
      .post("/api/compare/analyze")
      .send({ playerType: "hitter", playerIds, analysisMode: "straightforward" });

    expect(first.status).toBe(200);
    expect(first.body.cached).toBe(false);
    expect(second.status).toBe(200);
    expect(second.body.cached).toBe(true);
    expect(second.body.players).toHaveLength(2);
    expect(analysisHandler).toHaveBeenCalledTimes(1);
  });

  it("does not cache unavailable Claude responses", async () => {
    const memoryCache = createMemoryCacheStore();
    __setAnalysisCacheStoreForTests(memoryCache.store);
    const analysisHandler = vi.fn(async (prompt: string, persona: string) => ({
      prompt,
      persona,
      headline: "Analysis unavailable",
      analysis: "Anthropic API error: temporarily unavailable",
      cached: false,
    }));
    __setAnalysisHandlerForTests(analysisHandler);

    const { body: listBody } = await request(app).get("/api/players").query({ type: "hitter", q: "Judge" });
    const hitter = listBody.players[0];
    await request(app).post("/api/analyze").send({ playerId: hitter.id, playerType: "hitter", analysisMode: "gen_z" });
    await request(app).post("/api/analyze").send({ playerId: hitter.id, playerType: "hitter", analysisMode: "gen_z" });

    await settleCacheWrites();
    expect(analysisHandler).toHaveBeenCalledTimes(2);
    expect(memoryCache.rows.size).toBe(0);
  });

  it("does not cache malformed Claude responses", async () => {
    process.env.ANTHROPIC_API_KEY = "test-key";
    const memoryCache = createMemoryCacheStore();
    __setAnalysisCacheStoreForTests(memoryCache.store);
    const fetchSpy = vi.spyOn(globalThis, "fetch").mockImplementation(
      async () =>
        new Response(
          JSON.stringify({
            id: "msg_test",
            type: "message",
            role: "assistant",
            model: "claude-sonnet-5-5",
            content: [{ type: "text", text: "not json" }],
            stop_reason: "end_turn",
            stop_sequence: null,
            usage: { input_tokens: 1, output_tokens: 1 },
          }),
          { status: 200, headers: { "content-type": "application/json" } }
        )
    );

    const { body: listBody } = await request(app).get("/api/players").query({ type: "hitter", q: "Judge" });
    const hitter = listBody.players[0];
    await request(app).post("/api/analyze").send({ playerId: hitter.id, playerType: "hitter", analysisMode: "gen_z" });
    await request(app).post("/api/analyze").send({ playerId: hitter.id, playerType: "hitter", analysisMode: "gen_z" });

    await settleCacheWrites();
    expect(fetchSpy).toHaveBeenCalledTimes(2);
    expect(memoryCache.rows.size).toBe(0);
  });

  it("falls back to Claude when cache reads fail", async () => {
    __setAnalysisCacheStoreForTests({
      async get() {
        throw new Error("read failed");
      },
      async set() {},
      async prune() {},
    });
    const analysisHandler = vi.fn(async (prompt: string, persona: string) => ({
      prompt,
      persona,
      headline: "Fresh headline",
      analysis: "Fresh analysis",
      cached: false,
    }));
    __setAnalysisHandlerForTests(analysisHandler);

    const { body: listBody } = await request(app).get("/api/players").query({ type: "hitter", q: "Judge" });
    const hitter = listBody.players[0];
    const response = await request(app)
      .post("/api/analyze")
      .send({ playerId: hitter.id, playerType: "hitter", analysisMode: "gen_z" });

    expect(response.status).toBe(200);
    expect(response.body.analysis).toBe("Fresh analysis");
    expect(analysisHandler).toHaveBeenCalledTimes(1);
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
    const analysisHandler = vi.fn(async (prompt: string, persona: string) => ({
      prompt,
      persona,
      headline: "Fresh headline",
      analysis: "Fresh analysis",
      cached: false,
    }));
    __setAnalysisHandlerForTests(analysisHandler);

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
    __setAnalysisHandlerForTests(async (prompt: string, persona: string) => ({
      prompt,
      persona,
      headline: "Tracked headline",
      analysis: "Tracked analysis",
      cached: false,
    }));

    const { body: listBody } = await request(app).get("/api/players").query({ type: "hitter", q: "Judge" });
    const hitter = listBody.players[0];
    const { body: compareListBody } = await request(app).get("/api/players").query({ type: "hitter" });
    const comparePlayerIds = compareListBody.players.slice(0, 2).map((player: any) => player.id);
    await request(app)
      .post("/api/analyze")
      .set("x-session-id", "tracked-a")
      .send({ playerId: hitter.id, playerType: "hitter", analysisMode: "gen_z" });
    await settleCacheWrites();
    await request(app)
      .post("/api/analyze")
      .set("x-session-id", "tracked-b")
      .send({ playerId: hitter.id, playerType: "hitter", analysisMode: "gen_z" });
    await request(app)
      .post("/api/compare/analyze")
      .set("x-session-id", "tracked-c")
      .send({ playerType: "hitter", playerIds: comparePlayerIds, analysisMode: "gen_z" });

    await vi.waitFor(() => expect(fetchSpy).toHaveBeenCalledTimes(3));
    const loggedModes = fetchSpy.mock.calls.map(([_, init]) => {
      const payload = JSON.parse(String(init?.body ?? "{}"));
      return payload.events[0].event_properties.mode;
    });
    expect(loggedModes).toEqual(["single", "single", "compare"]);
  });

  it("does not log session starts while admin mode is enabled", async () => {
    process.env.ADMIN_PASSWORD = "top-secret";
    const fetchSpy = vi.spyOn(globalThis, "fetch").mockResolvedValue({ ok: true } as Response);

    const response = await request(app)
      .post("/api/sessions?admin=top-secret")
      .send({ sessionId: "admin-session" });

    expect(response.status).toBe(204);
    await new Promise((resolve) => setTimeout(resolve, 0));
    expect(fetchSpy).not.toHaveBeenCalled();
    delete process.env.ADMIN_PASSWORD;
  });

  it("does not log analysis events while admin mode is enabled", async () => {
    process.env.ADMIN_PASSWORD = "top-secret";
    const fetchSpy = vi.spyOn(globalThis, "fetch").mockResolvedValue({ ok: true } as Response);
    __setAnalysisHandlerForTests(async (prompt: string, persona: string) => ({
      prompt,
      persona,
      headline: "Admin headline",
      analysis: "Admin analysis",
      cached: false,
    }));

    const { body: listBody } = await request(app).get("/api/players").query({ type: "hitter", q: "Judge" });
    const hitter = listBody.players[0];
    const response = await request(app)
      .post("/api/analyze?admin=top-secret")
      .set("x-session-id", "admin-analysis")
      .send({ playerId: hitter.id, playerType: "hitter", analysisMode: "gen_z" });

    expect(response.status).toBe(200);
    await new Promise((resolve) => setTimeout(resolve, 0));
    expect(fetchSpy).not.toHaveBeenCalled();
    delete process.env.ADMIN_PASSWORD;
  });

  it("prunes stale analysis cache rows after successful writes", async () => {
    const memoryCache = createMemoryCacheStore();
    memoryCache.rows.set("old-cache-key", {
      dataThroughDate: "2001-04-01",
      headline: "Old headline",
      analysis: "Old analysis",
    });
    __setAnalysisCacheStoreForTests(memoryCache.store);
    __setAnalysisHandlerForTests(async (prompt: string, persona: string) => ({
      prompt,
      persona,
      headline: "Fresh headline",
      analysis: "Fresh analysis",
      cached: false,
    }));

    const { body: listBody } = await request(app).get("/api/players").query({ type: "hitter", q: "Judge" });
    const hitter = listBody.players[0];
    await request(app).post("/api/analyze").send({ playerId: hitter.id, playerType: "hitter", analysisMode: "gen_z" });

    await vi.waitFor(() => expect(memoryCache.rows.has("old-cache-key")).toBe(false));
    expect(memoryCache.rows.size).toBe(1);
  });

  it("prunes stale analysis cache rows once per data date", async () => {
    const memoryCache = createMemoryCacheStore();
    const pruneSpy = vi.spyOn(memoryCache.store, "prune");
    __setAnalysisCacheStoreForTests(memoryCache.store);
    __setAnalysisHandlerForTests(async (prompt: string, persona: string) => ({
      prompt,
      persona,
      headline: "Fresh headline",
      analysis: "Fresh analysis",
      cached: false,
    }));

    const { body: listBody } = await request(app).get("/api/players").query({ type: "hitter" });
    const [first, second] = listBody.players;
    const analyze = (playerId: string) =>
      request(app).post("/api/analyze").send({ playerId, playerType: "hitter", analysisMode: "gen_z" });

    await analyze(first.id);
    await settleCacheWrites();
    await analyze(second.id);
    await settleCacheWrites();
    expect(memoryCache.rows.size).toBe(2);
    expect(pruneSpy).toHaveBeenCalledTimes(1);

    __setDataFreshnessForTests({ dataThroughDate: "2099-04-22", dataThroughLabel: "April 22" });
    await analyze(first.id);
    await settleCacheWrites();
    expect(pruneSpy).toHaveBeenCalledTimes(2);
    expect(pruneSpy).toHaveBeenLastCalledWith("2099-04-22");
    expect(memoryCache.rows.size).toBe(1);
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

  it("returns a fantasy daily matchup start/sit decision for a hitter", async () => {
    const memoryCache = createMemoryCacheStore();
    __setAnalysisCacheStoreForTests(memoryCache.store);
    const { body: listBody } = await request(app).get("/api/players").query({ type: "hitter", q: "Judge" });
    const hitter = listBody.players[0];
    const mlbamid = Number(hitter.mlbamid);
    const fetchSpy = mockMlbApi({ playerMlbamId: mlbamid });
    const fantasyHandler = vi.fn(async (prompt: string) => ({
      prompt,
      decision: "START" as const,
      confidence: "Medium" as const,
      headline: "Start Judge for the platoon edge",
      analysis: "START Judge. The matchup context shows a right-handed bat facing a left-handed starter, with lineup status confirmed.",
      cached: false,
    }));
    __setFantasyDecisionHandlerForTests(fantasyHandler);

    const response = await request(app)
      .post("/api/fantasy/daily-matchup/analyze")
      .set("x-session-id", "fantasy-tracked")
      .send({ playerId: hitter.id, playerType: "hitter", date: "2026-04-25" });

    expect(response.status).toBe(200);
    expect(response.body.decision).toBe("START");
    expect(response.body.confidence).toBe("Medium");
    expect(response.body.matchup.opponent.name).toBe("Houston Astros");
    expect(response.body.matchup.opposingStarter.name).toBe("Ryan Weathers");
    expect(response.body.matchup.platoonLabel).toBe("RHB vs LHP");
    expect(response.body.matchup.lineupStatus).toBe("confirmed");
    expect(fantasyHandler).toHaveBeenCalledTimes(1);
    expect(fantasyHandler.mock.calls[0][0]).toContain("decision must be exactly START or SIT");
    expect(fantasyHandler.mock.calls[0][0]).toContain("Do not mention confirmed lineup status in the headline");
    expect(fantasyHandler.mock.calls[0][0]).toContain("Keep stat density low in the analysis");
    expect(fantasyHandler.mock.calls[0][0]).toContain("do not stack current value, baseline, raw diff, and league-adjusted diff");
    expect(fantasyHandler.mock.calls[0][0]).toContain("Fresh MLB matchup data");
    await vi.waitFor(() => {
      const amplitudeCall = fetchSpy.mock.calls.find(([input]) => String(input).includes("api2.amplitude.com"));
      expect(amplitudeCall).toBeTruthy();
      const payload = JSON.parse(String(amplitudeCall?.[1]?.body ?? "{}"));
      expect(payload.events[0].event_properties).toMatchObject({
        player_id: hitter.id,
        player_name: hitter.name,
        analysis_mode: "fantasy",
        mode: "fantasy",
        player_type: "hitter",
        event_type: "fantasy",
      });
    });
  });

  it("returns SIT guidance when a pitcher is not listed as today's probable starter", async () => {
    const { body: listBody } = await request(app).get("/api/players").query({ type: "pitcher", q: "Skubal" });
    const pitcher = listBody.players[0];
    const mlbamid = Number(pitcher.mlbamid);
    mockMlbApi({ playerMlbamId: mlbamid });
    __setFantasyDecisionHandlerForTests(async (prompt: string) => ({
      prompt,
      decision: "SIT",
      confidence: "High",
      headline: "Sit him because he is not the listed starter",
      analysis: "SIT. The selected pitcher is not listed as today's probable starter, so he should not be used for a daily start decision.",
      cached: false,
    }));

    const response = await request(app)
      .post("/api/fantasy/daily-matchup/analyze")
      .send({ playerId: pitcher.id, playerType: "pitcher", date: "2026-04-25" });

    expect(response.status).toBe(200);
    expect(response.body.decision).toBe("SIT");
    expect(response.body.matchup.isProbableStarter).toBe(false);
    expect(response.body.matchup.selectedTeamStarter.name).toBe("Mike Burrows");
  });

  it("marks selected pitchers as probable starters when MLB probable data matches", async () => {
    const { body: listBody } = await request(app).get("/api/players").query({ type: "pitcher", q: "Skubal" });
    const pitcher = listBody.players[0];
    const mlbamid = Number(pitcher.mlbamid);
    mockMlbApi({ playerMlbamId: mlbamid, selectedProbablePitcherId: mlbamid });
    __setFantasyDecisionHandlerForTests(async (prompt: string) => ({
      prompt,
      decision: "START",
      confidence: "Medium",
      headline: "Start the listed probable starter",
      analysis: "START. He is listed as the probable starter, so the matchup can be evaluated as an active pitching start.",
      cached: false,
    }));

    const response = await request(app)
      .post("/api/fantasy/daily-matchup/analyze")
      .send({ playerId: pitcher.id, playerType: "pitcher", date: "2026-04-25" });

    expect(response.status).toBe(200);
    expect(response.body.matchup.isProbableStarter).toBe(true);
  });

  it("returns no-game matchup context when MLB has no game for the selected player", async () => {
    const { body: listBody } = await request(app).get("/api/players").query({ type: "hitter", q: "Judge" });
    const hitter = listBody.players[0];
    const mlbamid = Number(hitter.mlbamid);
    vi.spyOn(globalThis, "fetch").mockImplementation(async (input: string | URL | Request) => {
      const url = String(input);
      if (url.includes("/api/v1/schedule")) {
        return jsonResponse({ dates: [{ games: [] }] });
      }
      if (url.includes("/api/v1/people")) {
        return jsonResponse({
          people: [
            {
              id: mlbamid,
              fullName: "Aaron Judge",
              currentTeam: { id: 147, name: "New York Yankees" },
              batSide: { code: "R" },
              pitchHand: { code: "R" },
            },
          ],
        });
      }
      return jsonResponse({});
    });
    __setFantasyDecisionHandlerForTests(async (prompt: string) => ({
      prompt,
      decision: "SIT",
      confidence: "High",
      headline: "Sit him with no game today",
      analysis: "SIT. No MLB game was found for this player today.",
      cached: false,
    }));

    const response = await request(app)
      .post("/api/fantasy/daily-matchup/analyze")
      .send({ playerId: hitter.id, playerType: "hitter", date: "2026-04-25" });

    expect(response.status).toBe(200);
    expect(response.body.matchup.matchupStatus).toBe("no_game");
    expect(response.body.decision).toBe("SIT");
  });

  it("returns persona metadata", async () => {
    const response = await request(app).get("/api/vibes");
    expect(response.status).toBe(200);
    expect(response.body.vibes).toHaveLength(9);
    expect(response.body.defaultMode).toBe("straightforward");
    expect(response.body.vibes[0]).toMatchObject({
      id: "straightforward",
      label: "Straightforward (Default)",
    });
    expect(response.body.vibes[0].description).toContain("friendly baseball person");
    expect(response.body.vibes[0].description).toContain("warm, conversational");
    expect(response.body.vibes[0].description).toContain("full, naturally flowing sentences");
    expect(response.body.vibes[0].description).toContain("inverted-pyramid structure");
    expect(response.body.vibes[0].description).toContain("metric-first sentence fragments");
  });

  it("requires admin access for social suggestions when admin password is configured", async () => {
    process.env.ADMIN_PASSWORD = "top-secret";

    const response = await request(app).get("/api/admin/social-suggestions");

    expect(response.status).toBe(403);
    expect(response.body.error).toContain("Admin access required");
    delete process.env.ADMIN_PASSWORD;
  });

  it("rejects the old unsigned admin cookie and wrong cookie values", async () => {
    process.env.ADMIN_PASSWORD = "top-secret";

    const forged = await request(app).get("/api/admin/social-suggestions").set("Cookie", "mcfarland_admin_mode=1");
    const wrong = await request(app).get("/api/admin/social-suggestions").set("Cookie", "mcfarland_admin_mode=not-the-token");

    expect(forged.status).toBe(403);
    expect(wrong.status).toBe(403);
  });

  it("issues a signed admin cookie that works on later requests", async () => {
    process.env.ADMIN_PASSWORD = "top-secret";

    const login = await request(app).get("/api/admin/social-suggestions?admin=top-secret");
    expect(login.status).toBe(200);

    const setCookie = ([] as string[]).concat(login.headers["set-cookie"] ?? []);
    const adminCookie = setCookie.find((cookie) => cookie.startsWith("mcfarland_admin_mode="));
    expect(adminCookie).toBeDefined();
    const cookieValue = adminCookie!.split(";")[0].split("=")[1];
    expect(cookieValue).not.toBe("1");
    expect(cookieValue).not.toContain("top-secret");

    const replay = await request(app).get("/api/admin/social-suggestions").set("Cookie", `mcfarland_admin_mode=${cookieValue}`);
    expect(replay.status).toBe(200);
  });

  it("invalidates admin cookies when the admin password changes", async () => {
    process.env.ADMIN_PASSWORD = "top-secret";
    const login = await request(app).get("/api/admin/social-suggestions?admin=top-secret");
    const setCookie = ([] as string[]).concat(login.headers["set-cookie"] ?? []);
    const cookie = setCookie.find((entry) => entry.startsWith("mcfarland_admin_mode="))!.split(";")[0];

    process.env.ADMIN_PASSWORD = "rotated-secret";
    const response = await request(app).get("/api/admin/social-suggestions").set("Cookie", cookie);

    expect(response.status).toBe(403);
  });

  it("fails closed for social suggestions in production without an admin password", async () => {
    vi.stubEnv("NODE_ENV", "production");
    delete process.env.ADMIN_PASSWORD;

    try {
      const response = await request(app).get("/api/admin/social-suggestions");

      expect(response.status).toBe(503);
      expect(response.body.error).toContain("Admin access is not configured");
    } finally {
      vi.unstubAllEnvs();
    }
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

  it("returns the best name matches first for player searches", async () => {
    const response = await request(app).get("/api/players").query({ type: "hitter", q: "ma", limit: 8 });

    expect(response.status).toBe(200);
    expect(response.body.players).toHaveLength(8);
    for (const player of response.body.players) {
      const words = player.name.toLowerCase().split(/\s+/);
      const lastName = /^(jr\.?|sr\.?|ii|iii|iv|v)$/.test(words[words.length - 1]) ? words[words.length - 2] : words[words.length - 1];
      expect(player.name.toLowerCase().startsWith("ma") || lastName.startsWith("ma")).toBe(true);
    }
    expect(Object.keys(response.body.players[0]).sort()).toEqual(["id", "mlbamid", "name", "type"]);
  });

  it("returns a JSON 404 for unknown API routes", async () => {
    const getResponse = await request(app).get("/api/nope");
    const postResponse = await request(app).post("/api/nope").send({});

    expect(getResponse.status).toBe(404);
    expect(getResponse.headers["content-type"]).toContain("application/json");
    expect(getResponse.body).toEqual({ error: "Not found" });
    expect(postResponse.status).toBe(404);
  });

  it("rejects oversized request bodies", async () => {
    const response = await request(app)
      .post("/api/sessions")
      .send({ sessionId: "x".repeat(64 * 1024) });

    expect(response.status).toBe(413);
  });

  it("returns trending quick links for hitters and pitchers", async () => {
    const response = await request(app).get("/api/trending");

    expect(response.status).toBe(200);
    expect(response.body.newsworthy).toBeInstanceOf(Array);
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

  it("accepts announcement analytics events", async () => {
    const response = await request(app)
      .post("/api/announcement-events")
      .send({ sessionId: "test-session", announcementId: "2026-11-search-ranking", action: "dismissed" });

    expect(response.status).toBe(204);
  });

  it("rejects announcement events with an unknown action", async () => {
    const response = await request(app)
      .post("/api/announcement-events")
      .send({ sessionId: "test-session", announcementId: "2026-11-search-ranking", action: "exploded" });

    expect(response.status).toBe(400);
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
    expect(response.text).toContain("og:image");
    expect(response.text).toContain("twitter:image");
    expect(response.text).toContain("/share-image.png?");
    expect(response.text).toContain('property="og:image:width" content="1200"');
    expect(response.text).toContain("Judge");
    expect(response.text).toContain("/?mode=single");
  });

  it("uses a cached generated headline in share social preview titles", async () => {
    const memoryCache = createMemoryCacheStore();
    __setAnalysisCacheStoreForTests(memoryCache.store);
    __setAnalysisHandlerForTests(async (prompt: string, persona: string) => ({
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
    await settleCacheWrites();
    const response = await request(app)
      .get("/share")
      .query({ mode: "single", playerType: "hitter", playerId: hitter.id, vibe: "gen_z" });

    expect(response.status).toBe(200);
    expect(response.text).toContain('property="og:title" content="Judge keeps forcing the issue | McFARLAND"');
    expect(response.text).toContain('name="twitter:title" content="Judge keeps forcing the issue | McFARLAND"');
  });

  it("does not include image metadata for plain app URL shares", async () => {
    const response = await request(app).get("/");

    expect(response.status).toBe(200);
    expect(response.headers["content-type"]).toContain("text/html");
    expect(response.text).toContain('property="og:title" content="McFARLAND | Advanced Baseball Analysis"');
    expect(response.text).toContain('name="twitter:card" content="summary"');
    expect(response.text).not.toContain('property="og:image"');
    expect(response.text).not.toContain('name="twitter:image"');
    expect(response.text).not.toContain("share-image.png");
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
