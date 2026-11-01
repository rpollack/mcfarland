import type { Announcement } from "../announcements";

const DISMISSED_STORAGE_KEY = "mcfarland-dismissed-announcements-v1";
const DAY_MS = 24 * 60 * 60 * 1000;

// Used when browser storage is blocked, so a dismissed banner stays gone for the rest of the session.
const sessionDismissedIds = new Set<string>();

function startMs(announcement: Announcement): number {
  return Date.parse(announcement.startsAt);
}

function endMs(announcement: Announcement): number {
  // endsAt is inclusive, so the announcement runs through the end of that UTC day.
  return Date.parse(announcement.endsAt) + DAY_MS;
}

export function isAnnouncementActive(announcement: Announcement, now: Date): boolean {
  const time = now.getTime();
  return time >= startMs(announcement) && time < endMs(announcement);
}

export function readDismissedAnnouncementIds(): Set<string> {
  const ids = new Set(sessionDismissedIds);
  if (typeof window === "undefined" || !window.localStorage) {
    return ids;
  }

  try {
    const raw = window.localStorage.getItem(DISMISSED_STORAGE_KEY);
    if (!raw) {
      return ids;
    }
    const parsed: unknown = JSON.parse(raw);
    if (Array.isArray(parsed)) {
      parsed.filter((value): value is string => typeof value === "string").forEach((id) => ids.add(id));
    }
  } catch (error) {
    console.warn("Unable to read dismissed announcements from localStorage", error);
  }
  return ids;
}

export function dismissAnnouncement(id: string): void {
  sessionDismissedIds.add(id);
  if (typeof window === "undefined" || !window.localStorage) {
    return;
  }

  try {
    const next = readDismissedAnnouncementIds();
    next.add(id);
    window.localStorage.setItem(DISMISSED_STORAGE_KEY, JSON.stringify([...next]));
  } catch (error) {
    console.warn("Unable to save dismissed announcement to localStorage", error);
  }
}

/** The single announcement to show: the most recently started one that is active and not dismissed. */
export function getActiveAnnouncement(
  announcements: Announcement[],
  now: Date,
  dismissedIds: Set<string>
): Announcement | null {
  const candidates = announcements
    .filter((announcement) => isAnnouncementActive(announcement, now) && !dismissedIds.has(announcement.id))
    .sort((a, b) => startMs(b) - startMs(a));
  return candidates[0] ?? null;
}

/** Every announcement that has started, newest first, for the "What's new" list. */
export function getAnnouncementHistory(announcements: Announcement[], now: Date): Announcement[] {
  return announcements
    .filter((announcement) => startMs(announcement) <= now.getTime())
    .sort((a, b) => startMs(b) - startMs(a));
}

export function __resetSessionDismissedForTests(): void {
  sessionDismissedIds.clear();
}
