import request from "supertest";
import { describe, expect, it, beforeAll } from "vitest";
import { createServer } from "../src/index.js";

const app = createServer();

describe("McFarland API", () => {
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
});
