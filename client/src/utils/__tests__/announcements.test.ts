import { afterEach, beforeEach, describe, expect, it } from "vitest";
import type { Announcement } from "../../announcements";
import {
  __resetSessionDismissedForTests,
  dismissAnnouncement,
  getActiveAnnouncement,
  getAnnouncementHistory,
  isAnnouncementActive,
  readDismissedAnnouncementIds,
} from "../announcements";

const older: Announcement = { id: "older", lead: "New:", text: "Older", startsAt: "2026-11-01", endsAt: "2026-11-30" };
const newer: Announcement = { id: "newer", lead: "New:", text: "Newer", startsAt: "2026-11-10", endsAt: "2026-11-30" };

describe("announcement scheduling", () => {
  it("is active from the start of the first day through the end of the last day (UTC)", () => {
    expect(isAnnouncementActive(older, new Date("2026-10-31T23:59:59Z"))).toBe(false);
    expect(isAnnouncementActive(older, new Date("2026-11-01T00:00:00Z"))).toBe(true);
    expect(isAnnouncementActive(older, new Date("2026-11-30T23:59:59Z"))).toBe(true);
    expect(isAnnouncementActive(older, new Date("2026-12-01T00:00:00Z"))).toBe(false);
  });

  it("shows only the most recently started active announcement", () => {
    const now = new Date("2026-11-15T12:00:00Z");
    expect(getActiveAnnouncement([older, newer], now, new Set())?.id).toBe("newer");
  });

  it("falls back to another active announcement when the newest is dismissed", () => {
    const now = new Date("2026-11-15T12:00:00Z");
    expect(getActiveAnnouncement([older, newer], now, new Set(["newer"]))?.id).toBe("older");
  });

  it("returns null when nothing is active or everything is dismissed", () => {
    expect(getActiveAnnouncement([older], new Date("2027-01-01T00:00:00Z"), new Set())).toBeNull();
    expect(getActiveAnnouncement([older], new Date("2026-11-15T00:00:00Z"), new Set(["older"]))).toBeNull();
  });

  it("lists started announcements newest first, including expired ones", () => {
    const history = getAnnouncementHistory([older, newer], new Date("2027-01-01T00:00:00Z"));
    expect(history.map((announcement) => announcement.id)).toEqual(["newer", "older"]);
    expect(getAnnouncementHistory([older, newer], new Date("2026-11-05T00:00:00Z")).map((a) => a.id)).toEqual(["older"]);
  });
});

describe("dismissed announcements", () => {
  beforeEach(() => {
    window.localStorage.clear();
    __resetSessionDismissedForTests();
  });

  afterEach(() => {
    window.localStorage.clear();
    __resetSessionDismissedForTests();
  });

  it("remembers dismissed ids across reads", () => {
    dismissAnnouncement("older");
    dismissAnnouncement("newer");
    expect([...readDismissedAnnouncementIds()].sort()).toEqual(["newer", "older"]);
  });

  it("ignores malformed stored data", () => {
    window.localStorage.setItem("mcfarland-dismissed-announcements-v1", "{not json");
    expect(readDismissedAnnouncementIds().size).toBe(0);
    window.localStorage.setItem("mcfarland-dismissed-announcements-v1", JSON.stringify([1, "ok", null]));
    expect([...readDismissedAnnouncementIds()]).toEqual(["ok"]);
  });

  it("keeps an announcement dismissed for the session when storage throws", () => {
    const original = Storage.prototype.setItem;
    Storage.prototype.setItem = () => {
      throw new Error("blocked");
    };
    try {
      dismissAnnouncement("older");
    } finally {
      Storage.prototype.setItem = original;
    }
    expect(readDismissedAnnouncementIds().has("older")).toBe(true);
  });
});
