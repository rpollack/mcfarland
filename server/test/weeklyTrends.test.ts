import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

const fake = vi.hoisted(() => ({
  queries: [] as string[],
  rankingDate: "2026-05-04",
  playerName: "Aaron Judge",
  gate: null as Promise<void> | null,
  query: null as null | ((sql: string) => Promise<{ rows: unknown[]; rowCount: number }>),
}));

vi.mock("../src/db.js", () => ({
  getPool: async () => ({
    query: (sql: string) => fake.query!(sql),
  }),
}));

import { __clearWeeklyTrendsCacheForTests, getWeeklyTrends, refreshDailyTrendData } from "../src/weeklyTrends.js";

function rankingQueries() {
  return fake.queries.filter((sql) => sql.includes("MAX(snapshot_date)"));
}

describe("weekly trends cache", () => {
  beforeEach(() => {
    vi.useFakeTimers({ toFake: ["Date"] });
    vi.setSystemTime(new Date("2026-05-04T12:00:00Z"));
    fake.queries = [];
    fake.rankingDate = "2026-05-04";
    fake.playerName = "Aaron Judge";
    fake.gate = null;
    fake.query = async (sql: string) => {
      fake.queries.push(sql);
      if (sql.includes("MAX(snapshot_date)")) {
        if (fake.gate) {
          await fake.gate;
        }
        return { rows: [{ snapshot_date: fake.rankingDate }], rowCount: 1 };
      }
      if (sql.includes("COUNT(*)")) {
        return { rows: [{ count: 1 }], rowCount: 1 };
      }
      if (sql.includes("FROM daily_trend_rankings")) {
        return {
          rows: [
            { player_type: "hitter", bucket: "risers", rank: 1, player_id: "1", player_name: fake.playerName, mlbamid: null },
          ],
          rowCount: 1,
        };
      }
      return { rows: [], rowCount: 0 };
    };
    __clearWeeklyTrendsCacheForTests();
  });

  afterEach(() => {
    vi.useRealTimers();
  });

  it("serves repeat requests from cache until the TTL expires", async () => {
    const first = await getWeeklyTrends();
    const second = await getWeeklyTrends();
    expect(second).toBe(first);
    expect(rankingQueries()).toHaveLength(1);

    vi.setSystemTime(new Date("2026-05-04T12:05:01Z"));
    const third = await getWeeklyTrends();

    expect(third).not.toBe(first);
    expect(rankingQueries()).toHaveLength(2);
  });

  it("clears the cache when the daily trend data is refreshed", async () => {
    const before = await getWeeklyTrends();
    expect(before.hitters.risers[0].name).toBe("Aaron Judge");

    fake.playerName = "Juan Soto";
    const stillCached = await getWeeklyTrends();
    expect(stillCached.hitters.risers[0].name).toBe("Aaron Judge");

    await refreshDailyTrendData("2026-05-04");
    const after = await getWeeklyTrends();

    expect(after.hitters.risers[0].name).toBe("Juan Soto");
  });

  it("does not cache a result that was being computed when a refresh landed", async () => {
    let release: () => void = () => {};
    fake.gate = new Promise<void>((resolve) => (release = resolve));
    const inFlight = getWeeklyTrends();
    await vi.waitFor(() => expect(rankingQueries()).toHaveLength(1));

    await refreshDailyTrendData("2026-05-04");
    release();
    await inFlight;
    fake.gate = null;
    await getWeeklyTrends();

    expect(rankingQueries()).toHaveLength(2);
  });
});
