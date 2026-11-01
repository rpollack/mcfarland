import request from "supertest";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { createServer } from "../src/index.js";
import { __clearShareImageCacheForTests, buildShareImageModel } from "../src/shareImage.js";
import { getPlayerById, listPlayers } from "../src/dataStore.js";
import type { HitterRecord, PitcherRecord } from "../src/types.js";

const app = createServer();
const hitters = listPlayers("hitter") as HitterRecord[];
const pitchers = listPlayers("pitcher") as PitcherRecord[];
const hitter = hitters.find((player) => player.xwOBA_cur !== null && player.wOBA_cur !== null)!;
const pitcher = pitchers.find((player) => player.xera_cur !== null && player.era_cur !== null)!;
const PNG_SIGNATURE = Buffer.from([0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]);

function binaryParser(res: NodeJS.ReadableStream, callback: (error: Error | null, body: Buffer) => void) {
  const chunks: Buffer[] = [];
  res.on("data", (chunk: Buffer) => chunks.push(chunk));
  res.on("end", () => callback(null, Buffer.concat(chunks)));
}

describe("buildShareImageModel", () => {
  const base = { vibeLabel: "Gen Z", dataThroughLabel: "October 30" };

  it("builds a single-hitter card with the xwOBA vs. wOBA stat", () => {
    const model = buildShareImageModel({ ...base, mode: "single", playerType: "hitter", players: [hitter], headline: "A big take" });

    expect(model.headline).toBe("A big take");
    expect(model.subjects).toEqual([{ name: hitter.Name, mlbamid: hitter.mlbamid ?? null }]);
    expect(model.statLine).toMatch(/^xwOBA \.\d{3} vs\. wOBA \.\d{3}$/);
    expect(model.prompt).toBe("What the numbers say, in plain English");
  });

  it("uses xERA vs. ERA for pitchers", () => {
    const model = buildShareImageModel({ ...base, mode: "single", playerType: "pitcher", players: [pitcher], headline: null });

    expect(model.statLine).toMatch(/^xERA \d+\.\d{2} vs\. ERA \d+\.\d{2}$/);
    expect(model.headline).toBe(`${pitcher.Name} Pitching Breakdown`);
  });

  it("asks who you would take for comparisons, without revealing any vote split", () => {
    const model = buildShareImageModel({ ...base, mode: "compare", playerType: "hitter", players: [hitters[0], hitters[1]], headline: null });

    expect(model.headline).toBe(`Who would you take: ${hitters[0].Name} or ${hitters[1].Name}?`);
    expect(model.prompt).toBe("Cast your vote and see how everyone else picked");
    expect(model.statLine).toBeNull();
    expect(JSON.stringify(model)).not.toMatch(/\d+%/);
  });

  it("lists three players with an Oxford comma", () => {
    const model = buildShareImageModel({
      ...base,
      mode: "compare",
      playerType: "hitter",
      players: [hitters[0], hitters[1], hitters[2]],
      headline: null,
    });

    expect(model.headline).toBe(`Who would you take: ${hitters[0].Name}, ${hitters[1].Name}, or ${hitters[2].Name}?`);
  });

  it("truncates very long headlines", () => {
    const model = buildShareImageModel({ ...base, mode: "single", playerType: "hitter", players: [hitter], headline: "word ".repeat(100) });

    expect(model.headline.length).toBeLessThanOrEqual(140);
    expect(model.headline.endsWith("…")).toBe(true);
  });
});

describe("GET /share-image.png", () => {
  beforeEach(() => {
    __clearShareImageCacheForTests();
    // Keep tests off the network: headshots fail, so the card falls back to initials.
    vi.stubGlobal("fetch", vi.fn().mockRejectedValue(new Error("offline")));
  });

  afterEach(() => {
    vi.unstubAllGlobals();
  });

  it("renders a PNG for a single player", async () => {
    const response = await request(app)
      .get("/share-image.png")
      .query({ mode: "single", playerType: "hitter", playerId: String(hitter.PlayerId), vibe: "gen_z" })
      .buffer(true)
      .parse(binaryParser);

    expect(response.status).toBe(200);
    expect(response.headers["content-type"]).toBe("image/png");
    expect(response.headers["cache-control"]).toContain("max-age=3600");
    expect((response.body as Buffer).subarray(0, 8).equals(PNG_SIGNATURE)).toBe(true);
    expect((response.body as Buffer).length).toBeGreaterThan(5000);
  });

  it("renders a PNG for a comparison", async () => {
    const response = await request(app)
      .get("/share-image.png")
      .query({ mode: "compare", playerType: "hitter", playerIds: `${hitters[0].PlayerId},${hitters[1].PlayerId}` })
      .buffer(true)
      .parse(binaryParser);

    expect(response.status).toBe(200);
    expect((response.body as Buffer).subarray(0, 8).equals(PNG_SIGNATURE)).toBe(true);
  });

  it("404s when no players are selected or they are unknown", async () => {
    expect((await request(app).get("/share-image.png")).status).toBe(404);
    expect((await request(app).get("/share-image.png").query({ mode: "single", playerType: "hitter", playerId: "nope" })).status).toBe(404);
    expect(
      (await request(app).get("/share-image.png").query({ mode: "compare", playerType: "hitter", playerIds: String(hitters[0].PlayerId) })).status
    ).toBe(404);
  });

  it("names the vibe the way the app does in share previews", async () => {
    const response = await request(app)
      .get("/share")
      .query({ mode: "single", playerType: "hitter", playerId: String(hitter.PlayerId), vibe: "old_coot" });

    expect(response.text).toContain("in Old-Timer mode");
  });

  it("is referenced from the share preview as the og:image", async () => {
    const response = await request(app)
      .get("/share")
      .query({ mode: "single", playerType: "hitter", playerId: String(hitter.PlayerId) });

    expect(getPlayerById("hitter", String(hitter.PlayerId))).not.toBeNull();
    expect(response.text).toMatch(/property="og:image" content="[^"]*\/share-image\.png\?[^"]*playerId=/);
  });
});
