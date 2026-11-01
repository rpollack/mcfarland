import request from "supertest";
import { describe, expect, it } from "vitest";
import { createServer } from "../src/index.js";
import { listPlayers } from "../src/dataStore.js";
import { findRelatedPlayers } from "../src/relatedPlayers.js";
import type { HitterRecord, PitcherRecord } from "../src/types.js";

// Daily refreshes change the data, so these assert properties of the result rather than specific players.
const hitter = listPlayers("hitter").find(
  (player) => (player as HitterRecord).PA_cur !== null && (player as HitterRecord).PA_cur! >= 300 && (player as HitterRecord).xwOBA_cur !== null
) as HitterRecord;
const pitcher = listPlayers("pitcher").find(
  (player) => (player as PitcherRecord).tbf !== null && (player as PitcherRecord).tbf! >= 300 && (player as PitcherRecord).xera_cur !== null
) as PitcherRecord;

describe("findRelatedPlayers", () => {
  it("returns similar-gap hitters and a rival that are all different players", () => {
    const related = findRelatedPlayers("hitter", String(hitter.PlayerId));

    expect(related).not.toBeNull();
    expect(related!.similarGap).toHaveLength(2);
    expect(related!.rival).not.toBeNull();

    const ids = [...related!.similarGap.map((player) => player.id), related!.rival!.id];
    expect(new Set(ids).size).toBe(3);
    expect(ids).not.toContain(String(hitter.PlayerId));
    related!.similarGap.forEach((player) => expect(player.type).toBe("hitter"));
  });

  it("orders similar-gap players by how close their gap is to the player's own", () => {
    const related = findRelatedPlayers("hitter", String(hitter.PlayerId))!;
    const selfGap = hitter.xwOBA_cur! - hitter.wOBA_cur!;
    const [closest, next] = related.similarGap;

    expect(Math.abs(closest.gap! - selfGap)).toBeLessThanOrEqual(Math.abs(next.gap! - selfGap));
  });

  it("only suggests players with a meaningful sample", () => {
    const related = findRelatedPlayers("hitter", String(hitter.PlayerId))!;
    const byId = new Map(listPlayers("hitter").map((player) => [String(player.PlayerId), player as HitterRecord]));

    [...related.similarGap, related.rival!].forEach((player) => {
      expect(byId.get(player.id)!.PA_cur!).toBeGreaterThanOrEqual(300);
    });
  });

  it("works for pitchers", () => {
    const related = findRelatedPlayers("pitcher", String(pitcher.PlayerId));

    expect(related).not.toBeNull();
    expect(related!.similarGap.length).toBeGreaterThan(0);
    related!.similarGap.forEach((player) => expect(player.type).toBe("pitcher"));
  });

  it("returns null for an unknown player", () => {
    expect(findRelatedPlayers("hitter", "does-not-exist")).toBeNull();
  });
});

describe("GET /api/players/:playerId/related", () => {
  const app = createServer();

  it("returns related players", async () => {
    const response = await request(app).get(`/api/players/${hitter.PlayerId}/related`).query({ type: "hitter" });

    expect(response.status).toBe(200);
    expect(response.body.related.similarGap).toHaveLength(2);
  });

  it("requires a player type", async () => {
    const response = await request(app).get(`/api/players/${hitter.PlayerId}/related`);

    expect(response.status).toBe(400);
  });

  it("404s for an unknown player", async () => {
    const response = await request(app).get("/api/players/does-not-exist/related").query({ type: "hitter" });

    expect(response.status).toBe(404);
  });
});
