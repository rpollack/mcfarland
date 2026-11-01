import type { PlayerType } from "../types";

const STARRED_STORAGE_KEY = "mcfarland-starred-players-v1";
const STARRED_UPDATED_EVENT = "mcfarland-starred-players-updated";
const MAX_STARRED_PLAYERS = 100;

export type StarredPlayer = {
  playerId: string;
  playerName: string;
  playerType: PlayerType;
  mlbamid?: string | null;
  starredAt: string;
};

// Used when browser storage is blocked, so a star still works for the rest of the session.
let sessionStarred: StarredPlayer[] = [];

function isStarredPlayer(value: unknown): value is StarredPlayer {
  if (!value || typeof value !== "object") {
    return false;
  }
  const record = value as Record<string, unknown>;
  return (
    typeof record.playerId === "string" &&
    typeof record.playerName === "string" &&
    (record.playerType === "hitter" || record.playerType === "pitcher") &&
    typeof record.starredAt === "string"
  );
}

export function readStarredPlayers(): StarredPlayer[] {
  if (typeof window === "undefined" || !window.localStorage) {
    return sessionStarred;
  }

  try {
    const raw = window.localStorage.getItem(STARRED_STORAGE_KEY);
    if (!raw) {
      return sessionStarred;
    }
    const parsed: unknown = JSON.parse(raw);
    return Array.isArray(parsed) ? parsed.filter(isStarredPlayer) : [];
  } catch (error) {
    console.warn("Unable to read starred players from localStorage", error);
    return sessionStarred;
  }
}

export function isPlayerStarred(playerId: string, playerType: PlayerType): boolean {
  return readStarredPlayers().some((entry) => entry.playerId === playerId && entry.playerType === playerType);
}

/** Stars the player if they are not starred, otherwise removes the star. Returns the new starred state. */
export function toggleStarredPlayer(player: Omit<StarredPlayer, "starredAt">): boolean {
  const current = readStarredPlayers();
  const alreadyStarred = current.some((entry) => entry.playerId === player.playerId && entry.playerType === player.playerType);

  const next = alreadyStarred
    ? current.filter((entry) => !(entry.playerId === player.playerId && entry.playerType === player.playerType))
    : [{ ...player, starredAt: new Date().toISOString() }, ...current].slice(0, MAX_STARRED_PLAYERS);

  sessionStarred = next;
  if (typeof window !== "undefined" && window.localStorage) {
    try {
      window.localStorage.setItem(STARRED_STORAGE_KEY, JSON.stringify(next));
    } catch (error) {
      console.warn("Unable to write starred players to localStorage", error);
    }
  }
  if (typeof window !== "undefined") {
    window.dispatchEvent(new Event(STARRED_UPDATED_EVENT));
  }

  return !alreadyStarred;
}

export function getStarredPlayersUpdatedEventName(): string {
  return STARRED_UPDATED_EVENT;
}

export function __resetSessionStarredForTests(): void {
  sessionStarred = [];
}
