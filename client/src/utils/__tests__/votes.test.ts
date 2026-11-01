import { describe, expect, it } from "vitest";
import { computeVoteShares } from "../votes";

describe("computeVoteShares", () => {
  it("returns zeros when nobody has voted", () => {
    expect(computeVoteShares(["a", "b"], {})).toEqual([
      { id: "a", votes: 0, percent: 0 },
      { id: "b", votes: 0, percent: 0 },
    ]);
  });

  it("computes percentages in the order of the ids", () => {
    const shares = computeVoteShares(["a", "b"], { a: 3, b: 1 });
    expect(shares.map((share) => share.percent)).toEqual([75, 25]);
    expect(shares.map((share) => share.votes)).toEqual([3, 1]);
  });

  it("always adds up to 100 even when the split does not divide evenly", () => {
    for (const counts of [{ a: 1, b: 1, c: 1 }, { a: 2, b: 1, c: 0 }, { a: 5, b: 7, c: 11 }]) {
      const shares = computeVoteShares(["a", "b", "c"], counts);
      expect(shares.reduce((sum, share) => sum + share.percent, 0)).toBe(100);
    }
  });
});
