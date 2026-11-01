import { beforeEach, describe, expect, it } from "vitest";
import {
  __resetSessionStarredForTests,
  isPlayerStarred,
  readStarredPlayers,
  toggleStarredPlayer,
} from "../starredPlayers";

const judge = { playerId: "15640", playerName: "Aaron Judge", playerType: "hitter" as const, mlbamid: "592450" };

describe("starred players", () => {
  beforeEach(() => {
    window.localStorage.clear();
    __resetSessionStarredForTests();
  });

  it("stars and unstars a player", () => {
    expect(isPlayerStarred("15640", "hitter")).toBe(false);
    expect(toggleStarredPlayer(judge)).toBe(true);
    expect(isPlayerStarred("15640", "hitter")).toBe(true);
    expect(toggleStarredPlayer(judge)).toBe(false);
    expect(isPlayerStarred("15640", "hitter")).toBe(false);
  });

  it("keeps hitters and pitchers with the same id separate", () => {
    toggleStarredPlayer(judge);
    expect(isPlayerStarred("15640", "pitcher")).toBe(false);
  });

  it("lists the newest star first", () => {
    toggleStarredPlayer(judge);
    toggleStarredPlayer({ playerId: "1", playerName: "Next Player", playerType: "pitcher" });
    expect(readStarredPlayers().map((entry) => entry.playerName)).toEqual(["Next Player", "Aaron Judge"]);
  });

  it("caps the list at 100 players, dropping the oldest", () => {
    for (let index = 0; index < 105; index += 1) {
      toggleStarredPlayer({ playerId: String(index), playerName: `Player ${index}`, playerType: "hitter" });
    }
    const starred = readStarredPlayers();
    expect(starred).toHaveLength(100);
    expect(starred[0].playerId).toBe("104");
    expect(isPlayerStarred("0", "hitter")).toBe(false);
  });

  it("ignores malformed stored data", () => {
    window.localStorage.setItem("mcfarland-starred-players-v1", "{nope");
    expect(readStarredPlayers()).toEqual([]);
    window.localStorage.setItem("mcfarland-starred-players-v1", JSON.stringify([{ playerId: 1 }, null]));
    expect(readStarredPlayers()).toEqual([]);
  });

  it("still works for the session when storage is blocked", () => {
    const original = Storage.prototype.setItem;
    Storage.prototype.setItem = () => {
      throw new Error("blocked");
    };
    try {
      toggleStarredPlayer(judge);
    } finally {
      Storage.prototype.setItem = original;
    }
    window.localStorage.clear();
    expect(isPlayerStarred("15640", "hitter")).toBe(true);
  });
});
