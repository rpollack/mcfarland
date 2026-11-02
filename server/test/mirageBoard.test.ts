import request from "supertest";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { createServer } from "../src/index.js";
import { __setAnalysisCacheStoreForTests } from "../src/analysisCache.js";
import { __setAnalysisHandlerForTests } from "../src/claude.js";
import { getDataFreshness, listPlayers } from "../src/dataStore.js";
import { MIN_FEATURED_SAMPLE, getSampleSize } from "../src/luckGap.js";
import { buildMirageBoard, buildMirageBoardPrompt } from "../src/mirageBoard.js";
import type { PlayerType } from "../src/types.js";

const app = createServer();

function memoryCache() {
  const rows = new Map<string, { dataThroughDate: string; headline: string; analysis: string }>();
  return {
    async get({ cacheKey, dataThroughDate }: { cacheKey: string; dataThroughDate: string }) {
      const row = rows.get(cacheKey);
      return row && row.dataThroughDate === dataThroughDate ? { headline: row.headline, analysis: row.analysis } : null;
    },
    async set(entry: { cacheKey: string; dataThroughDate: string; headline: string; analysis: string }) {
      rows.set(entry.cacheKey, entry);
    },
    async prune() {},
  };
}

describe.each<PlayerType>(["hitter", "pitcher"])("buildMirageBoard (%s)", (type) => {
  const board = buildMirageBoard(type);

  it("lists up to five on each side, with the biggest gaps first", () => {
    expect(board.lucky.length).toBeGreaterThan(0);
    expect(board.unlucky.length).toBeGreaterThan(0);
    expect(board.lucky.length).toBeLessThanOrEqual(5);
    expect(board.unlucky.length).toBeLessThanOrEqual(5);

    board.lucky.forEach((row) => expect(row.gap).toBeLessThan(0));
    board.unlucky.forEach((row) => expect(row.gap).toBeGreaterThan(0));
    const lucky = board.lucky.map((row) => row.gap);
    const unlucky = board.unlucky.map((row) => row.gap);
    expect(lucky).toEqual([...lucky].sort((a, b) => a - b));
    expect(unlucky).toEqual([...unlucky].sort((a, b) => b - a));
  });

  it("only features players with a meaningful sample and never repeats a player", () => {
    const sampleById = new Map(listPlayers(type).map((player) => [String(player.PlayerId), getSampleSize(type, player)]));
    const rows = [...board.lucky, ...board.unlucky];

    rows.forEach((row) => expect(sampleById.get(row.id)).toBeGreaterThanOrEqual(MIN_FEATURED_SAMPLE));
    expect(new Set(rows.map((row) => row.id)).size).toBe(rows.length);
  });

  it("reports the season the data covers", () => {
    expect(board.season).toBe(Number(getDataFreshness().dataThroughDate.slice(0, 4)));
    expect(board.playerType).toBe(type);
  });
});

describe("buildMirageBoardPrompt", () => {
  it("describes the board in the right stats for hitters and pitchers", () => {
    const hitterBoard = buildMirageBoard("hitter");
    const hitterPrompt = buildMirageBoardPrompt(hitterBoard);
    expect(hitterPrompt).toContain(`${hitterBoard.season} "mirage board"`);
    expect(hitterPrompt).toContain("xwOBA");
    expect(hitterPrompt).toContain(hitterBoard.lucky[0].name);
    expect(hitterPrompt).toContain(hitterBoard.unlucky[0].name);

    const pitcherPrompt = buildMirageBoardPrompt(buildMirageBoard("pitcher"));
    expect(pitcherPrompt).toContain("xERA");
    expect(pitcherPrompt).not.toContain("xwOBA ");
  });

  it("keeps the model to the season that happened", () => {
    const prompt = buildMirageBoardPrompt(buildMirageBoard("hitter"));
    expect(prompt).toContain("exactly 3 short paragraphs");
    expect(prompt).toContain("Do not predict next season");
  });
});

describe("mirage board routes", () => {
  beforeEach(() => {
    __setAnalysisCacheStoreForTests(memoryCache());
  });

  afterEach(() => {
    __setAnalysisHandlerForTests(null);
    __setAnalysisCacheStoreForTests(null);
  });

  it("returns the board and defaults to hitters", async () => {
    const response = await request(app).get("/api/mirage-board");

    expect(response.status).toBe(200);
    expect(response.body.board.playerType).toBe("hitter");
    expect(response.body.board.lucky.length).toBeGreaterThan(0);
  });

  it("returns the pitcher board and rejects an unknown type", async () => {
    expect((await request(app).get("/api/mirage-board").query({ type: "pitcher" })).body.board.playerType).toBe("pitcher");
    expect((await request(app).get("/api/mirage-board").query({ type: "catcher" })).status).toBe(400);
  });

  it("writes the intro once per voice and serves repeats from the cache", async () => {
    const handler = vi.fn(async (prompt: string, persona: string) => ({
      prompt,
      persona,
      headline: "Luck evens out",
      analysis: "Para one.\n\nPara two.\n\nPara three.",
      cached: false,
    }));
    __setAnalysisHandlerForTests(handler);

    const first = await request(app).post("/api/mirage-board/intro").send({ playerType: "hitter", analysisMode: "gen_z" });
    expect(first.status).toBe(200);
    expect(first.body.headline).toBe("Luck evens out");
    expect(first.body.cached).toBe(false);
    await new Promise((resolve) => setImmediate(resolve));

    const second = await request(app).post("/api/mirage-board/intro").send({ playerType: "hitter", analysisMode: "gen_z" });
    expect(second.body.cached).toBe(true);
    expect(handler).toHaveBeenCalledTimes(1);

    await request(app).post("/api/mirage-board/intro").send({ playerType: "hitter", analysisMode: "old_coot" });
    expect(handler).toHaveBeenCalledTimes(2);
  });

  it("reports the service as unavailable when the model call fails", async () => {
    __setAnalysisHandlerForTests(async () => {
      throw new Error("model down");
    });

    const response = await request(app).post("/api/mirage-board/intro").send({ playerType: "hitter" });
    expect(response.status).toBe(502);
  });

  it("treats an unavailable analysis as an error instead of intro text", async () => {
    __setAnalysisHandlerForTests(async (prompt: string, persona: string) => ({
      prompt,
      persona,
      headline: "Analysis unavailable",
      analysis: "Anthropic API key is not configured.",
      cached: false,
    }));

    const response = await request(app).post("/api/mirage-board/intro").send({ playerType: "hitter" });
    expect(response.status).toBe(502);
  });

  it("rejects an invalid intro request", async () => {
    expect((await request(app).post("/api/mirage-board/intro").send({ playerType: "catcher" })).status).toBe(400);
  });
});
