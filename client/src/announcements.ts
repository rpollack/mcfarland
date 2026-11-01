export type Announcement = {
  /** Stable id. A dismissed announcement stays dismissed by id, so never reuse one. */
  id: string;
  /** Short lead-in shown in bold, e.g. "New:". */
  lead: string;
  text: string;
  /** Optional in-app destination, e.g. "/?mode=compare". */
  link?: { label: string; to: string };
  /** First day to show it (YYYY-MM-DD, UTC). */
  startsAt: string;
  /** Last day to show it, inclusive (YYYY-MM-DD, UTC). */
  endsAt: string;
};

/**
 * Add new announcements to the top of this list. Only one shows at a time (the most recently started
 * one that is active and not dismissed), and each expires on its own end date.
 */
export const ANNOUNCEMENTS: Announcement[] = [
  {
    id: "2026-11-mirage-board",
    lead: "New:",
    text: "The 2026 mirage board shows which hitters and pitchers ran lucky or unlucky this season.",
    link: { label: "See the board", to: "/" },
    startsAt: "2026-11-01",
    endsAt: "2026-11-30",
  },
  {
    id: "2026-11-search-ranking",
    lead: "New:",
    text: "Player search now puts exact name and last-name matches first, so the right player shows up sooner.",
    startsAt: "2026-11-01",
    endsAt: "2026-11-30",
  },
];
