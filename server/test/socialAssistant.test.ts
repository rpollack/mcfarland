import { afterEach, describe, expect, it, vi } from "vitest";
import { __setDataFreshnessForTests, getDataFreshness } from "../src/dataStore.js";
import {
  __clearNewsworthyQuickLinkCacheForTests,
  __clearPreviousSeasonSampleCacheForTests,
  __getBreakoutBaselineEligibilityForTests,
  __getPitcherBreakoutPreviousSeasonThresholdForTests,
  __setNewsDrivenCandidatePoolFetcherForTests,
  buildNewsDrivenCandidates,
  getTrendingQuickLinks,
} from "../src/socialAssistant.js";

const originalFreshness = getDataFreshness();

describe("social assistant news discovery", () => {
  afterEach(() => {
    __clearNewsworthyQuickLinkCacheForTests();
    __clearPreviousSeasonSampleCacheForTests();
    __setNewsDrivenCandidatePoolFetcherForTests(null);
    __setDataFreshnessForTests(originalFreshness);
  });

  const playerIndex = [
    {
      playerId: "1",
      playerType: "hitter",
      playerName: "Aaron Judge",
      score: 3,
      whyNow: "Aaron Judge stands out because wOBA +.050 vs weighted baseline over 120 PA.",
      statSnapshot: "120 PA · wOBA +.050",
      shareUrl: "https://example.test/share?playerId=1",
      news: [],
    },
    {
      playerId: "2",
      playerType: "pitcher",
      playerName: "Tarik Skubal",
      score: 4,
      whyNow: "Tarik Skubal stands out because xERA -0.55 vs weighted baseline over 150 TBF.",
      statSnapshot: "150 TBF · xERA -0.55",
      shareUrl: "https://example.test/share?playerId=2",
      news: [],
    },
  ] as const;

  it("matches news headlines to McFARLAND players before scoring candidates", () => {
    const candidates = buildNewsDrivenCandidates(
      [
        {
          query: "MLB hot streak player",
          title: "Aaron Judge keeps powering Yankees hot streak",
          link: "https://mlb.example/judge",
          source: "MLB.com",
          publishedAt: new Date().toUTCString(),
        },
        {
          query: "MLB strikeouts pitcher",
          title: "Tarik Skubal piles up strikeouts again",
          link: "https://detroit.example/skubal",
          source: "Detroit News",
          publishedAt: new Date().toUTCString(),
        },
      ],
      [...playerIndex]
    );

    expect(candidates.map((candidate) => candidate.playerName)).toEqual(["Tarik Skubal", "Aaron Judge"]);
    expect(candidates[0].news).toHaveLength(1);
    expect(candidates[0].whyNow).toContain("today's MLB news cycle");
    expect(candidates[0].whyNow).toContain("McFARLAND adds this angle");
  });

  it("deduplicates same-source mentions for the same player", () => {
    const candidates = buildNewsDrivenCandidates(
      [
        {
          query: "MLB hot streak player",
          title: "Aaron Judge powers another hot streak",
          link: "https://mlb.example/judge-1",
          source: "MLB.com",
          publishedAt: new Date().toUTCString(),
        },
        {
          query: "MLB homer",
          title: "Aaron Judge homer keeps Yankees rolling",
          link: "https://mlb.example/judge-2",
          source: "MLB.com",
          publishedAt: new Date().toUTCString(),
        },
      ],
      [...playerIndex]
    );

    expect(candidates).toHaveLength(1);
    expect(candidates[0].playerName).toBe("Aaron Judge");
    expect(candidates[0].news).toHaveLength(1);
  });

  it("caches newsworthy quick links for the current data day", async () => {
    __setDataFreshnessForTests({ dataThroughDate: "2026-05-04", dataThroughLabel: "May 4" });
    const fetcher = vi.fn(async () => [
      {
        playerId: "1",
        playerType: "hitter" as const,
        playerName: "Aaron Judge",
        score: 10,
        whyNow: "Aaron Judge is in the news.",
        statSnapshot: "120 PA · wOBA +.050",
        shareUrl: "https://example.test/share?playerId=1",
        news: [],
      },
    ]);
    __setNewsDrivenCandidatePoolFetcherForTests(fetcher);

    const first = await getTrendingQuickLinks("https://example.test");
    const second = await getTrendingQuickLinks("https://example.test");

    expect(first.newsworthy).toEqual([{ id: "1", name: "Aaron Judge", type: "hitter", mlbamid: null }]);
    expect(second.newsworthy).toEqual(first.newsworthy);
    expect(fetcher).toHaveBeenCalledTimes(1);
  });

  it("refreshes newsworthy quick links when data day changes", async () => {
    const fetcher = vi
      .fn()
      .mockResolvedValueOnce([
        {
          playerId: "1",
          playerType: "hitter" as const,
          playerName: "Aaron Judge",
          score: 10,
          whyNow: "Aaron Judge is in the news.",
          statSnapshot: "120 PA · wOBA +.050",
          shareUrl: "https://example.test/share?playerId=1",
          news: [],
        },
      ])
      .mockResolvedValueOnce([
        {
          playerId: "2",
          playerType: "pitcher" as const,
          playerName: "Tarik Skubal",
          score: 10,
          whyNow: "Tarik Skubal is in the news.",
          statSnapshot: "150 TBF · xERA -.055",
          shareUrl: "https://example.test/share?playerId=2",
          news: [],
        },
      ]);
    __setNewsDrivenCandidatePoolFetcherForTests(fetcher);

    __setDataFreshnessForTests({ dataThroughDate: "2026-05-04", dataThroughLabel: "May 4" });
    const first = await getTrendingQuickLinks("https://example.test");

    __setDataFreshnessForTests({ dataThroughDate: "2026-05-05", dataThroughLabel: "May 5" });
    const second = await getTrendingQuickLinks("https://example.test");

    expect(first.newsworthy[0]).toMatchObject({ id: "1", name: "Aaron Judge" });
    expect(second.newsworthy[0]).toMatchObject({ id: "2", name: "Tarik Skubal" });
    expect(fetcher).toHaveBeenCalledTimes(2);
  });

  it("requires half-season previous-year samples for breakout quick links", async () => {
    const quickLinks = await getTrendingQuickLinks("https://example.test");

    quickLinks.hitters.breakouts.forEach((player) => {
      const eligibility = __getBreakoutBaselineEligibilityForTests("hitter", player.id);

      expect(eligibility).not.toBeNull();
      expect(eligibility?.qualified).toBe(true);
      expect(eligibility?.sample).toBeGreaterThanOrEqual(300);
      expect(eligibility?.threshold).toBe(300);
    });

    quickLinks.pitchers.breakouts.forEach((player) => {
      const eligibility = __getBreakoutBaselineEligibilityForTests("pitcher", player.id);

      expect(eligibility).not.toBeNull();
      expect(eligibility?.qualified).toBe(true);
      expect(eligibility?.sample).toBeGreaterThanOrEqual(eligibility?.threshold ?? Number.POSITIVE_INFINITY);
      expect([125, 350]).toContain(eligibility?.threshold);
    });
  });

  it("uses different previous-year TBF thresholds for starter and reliever breakouts", () => {
    expect(__getPitcherBreakoutPreviousSeasonThresholdForTests(32, 32)).toBe(350);
    expect(__getPitcherBreakoutPreviousSeasonThresholdForTests(65, 0)).toBe(125);
  });
});
