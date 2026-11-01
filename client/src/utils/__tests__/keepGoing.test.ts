import { describe, expect, it } from "vitest";
import { buildCompareHref, formatLuckGap } from "../keepGoing";

describe("formatLuckGap", () => {
  it("formats hitter gaps in wOBA points with a sign and no leading zero", () => {
    expect(formatLuckGap(0.0274, "hitter")).toBe("+.027");
    expect(formatLuckGap(-0.012, "hitter")).toBe("−.012");
    expect(formatLuckGap(0, "hitter")).toBe(".000");
  });

  it("formats pitcher gaps in runs with two decimals", () => {
    expect(formatLuckGap(0.314, "pitcher")).toBe("+0.31");
    expect(formatLuckGap(-1.5, "pitcher")).toBe("−1.50");
  });
});

describe("buildCompareHref", () => {
  it("builds a compare link that the app can open directly", () => {
    expect(buildCompareHref("hitter", ["15640", "9"])).toBe("/?mode=compare&playerType=hitter&playerIds=15640%2C9");
  });
});
