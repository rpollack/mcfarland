import { describe, expect, it } from "vitest";
import { getPlayerSummaries } from "../src/dataStore.js";

// Independent restatement of the ranking rule, used to check the real data rather than specific names.
function lastNameOf(lowerName: string): string {
  const words = lowerName.split(/\s+/);
  while (words.length > 1 && ["jr", "jr.", "sr", "sr.", "ii", "iii", "iv", "v"].includes(words[words.length - 1])) {
    words.pop();
  }
  return words[words.length - 1];
}

function tierOf(name: string, query: string): 0 | 1 | 2 | null {
  const lower = name.toLowerCase();
  if (!lower.includes(query)) {
    return null;
  }
  if (lower.startsWith(query) || lastNameOf(lower).startsWith(query)) {
    return 0;
  }
  return lower.split(/[^\p{L}\p{N}]+/u).some((word) => word.startsWith(query)) ? 1 : 2;
}

const allHitters = getPlayerSummaries("hitter", undefined, 100_000);
const QUERIES = ["ma", "an", "lo", "tr", "ro", "al"];

describe("player search ranking", () => {
  it.each(QUERIES)("ranks prefix and last-name matches before mid-word matches for %s", (query) => {
    const results = getPlayerSummaries("hitter", query, 100_000);
    const tiers = results.map((player) => tierOf(player.name, query));

    expect(tiers.every((tier) => tier !== null)).toBe(true);
    // Guards against a vacuous pass: the real data has both strong and weak matches for these queries.
    expect(new Set(tiers).size).toBeGreaterThan(1);
    expect(tiers).toEqual([...tiers].sort((a, b) => (a as number) - (b as number)));
  });

  it.each(QUERIES)("keeps file order within a tier for %s", (query) => {
    const ranked = getPlayerSummaries("hitter", query, 100_000);
    const fileOrder = allHitters.filter((player) => tierOf(player.name, query) !== null);

    for (const tier of [0, 1, 2]) {
      const rankedIds = ranked.filter((player) => tierOf(player.name, query) === tier).map((player) => player.id);
      const expectedIds = fileOrder.filter((player) => tierOf(player.name, query) === tier).map((player) => player.id);
      expect(rankedIds).toEqual(expectedIds);
    }
  });

  it("returns every match that the old substring search returned", () => {
    for (const query of QUERIES) {
      const ranked = getPlayerSummaries("hitter", query, 100_000).map((player) => player.id).sort();
      const substring = allHitters
        .filter((player) => player.name.toLowerCase().includes(query))
        .map((player) => player.id)
        .sort();
      expect(ranked).toEqual(substring);
    }
  });

  it("applies the limit after ranking, so top results are the best matches", () => {
    const top = getPlayerSummaries("hitter", "ma", 8);

    expect(top).toHaveLength(8);
    expect(top.every((player) => tierOf(player.name, "ma") === 0)).toBe(true);
  });

  it("ignores case and surrounding whitespace in the query", () => {
    expect(getPlayerSummaries("hitter", "  MA ", 20)).toEqual(getPlayerSummaries("hitter", "ma", 20));
  });

  it("returns plain summaries in file order when there is no query", () => {
    const first = getPlayerSummaries("hitter", undefined, 3);

    expect(first).toEqual(allHitters.slice(0, 3));
    expect(Object.keys(first[0]).sort()).toEqual(["id", "mlbamid", "name", "type"]);
    expect(getPlayerSummaries("hitter", "   ", 3)).toEqual(first);
  });

  it("filters by player type", () => {
    const results = getPlayerSummaries("pitcher", "ma", 100_000);

    expect(results.length).toBeGreaterThan(0);
    expect(results.every((player) => player.type === "pitcher")).toBe(true);
  });
});
