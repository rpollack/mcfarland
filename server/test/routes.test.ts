import request from "supertest";
import { describe, expect, it, beforeAll } from "vitest";
import { createServer } from "../src/index.js";

const app = createServer();

describe("McFARLAND API", () => {
  beforeAll(() => {
    process.env.OPENAI_API_KEY = ""; // ensure offline tests do not call the API
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
