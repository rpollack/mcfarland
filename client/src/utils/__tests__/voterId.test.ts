import { beforeEach, describe, expect, it } from "vitest";
import { __resetVoterIdForTests, getOrCreateVoterId } from "../voterId";

describe("getOrCreateVoterId", () => {
  beforeEach(() => {
    window.localStorage.clear();
    __resetVoterIdForTests();
  });

  it("creates an id once and keeps returning it", () => {
    const first = getOrCreateVoterId();
    expect(first.length).toBeGreaterThanOrEqual(16);
    expect(getOrCreateVoterId()).toBe(first);
  });

  it("reuses the stored id after a reload", () => {
    const first = getOrCreateVoterId();
    __resetVoterIdForTests();
    expect(getOrCreateVoterId()).toBe(first);
  });

  it("is kept separate from the analytics session id", () => {
    window.localStorage.setItem("mcfarland-session-id", "analytics-session");
    expect(getOrCreateVoterId()).not.toBe("analytics-session");
  });

  it("still returns a stable id for the session when storage is blocked", () => {
    const original = Storage.prototype.getItem;
    const originalSet = Storage.prototype.setItem;
    Storage.prototype.getItem = () => {
      throw new Error("blocked");
    };
    Storage.prototype.setItem = () => {
      throw new Error("blocked");
    };
    try {
      const id = getOrCreateVoterId();
      expect(id.length).toBeGreaterThanOrEqual(16);
      expect(getOrCreateVoterId()).toBe(id);
    } finally {
      Storage.prototype.getItem = original;
      Storage.prototype.setItem = originalSet;
    }
  });
});
