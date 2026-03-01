import type { PlayerType } from "../types";

const RECENT_ANALYSES_STORAGE_KEY = "mcfarland-recent-analyses-v1";
const RECENT_ANALYSES_UPDATED_EVENT = "mcfarland-recent-analyses-updated";
const MAX_RECENT_ANALYSES = 3;

export type RecentAnalysisEntry = {
  playerId: string;
  playerName: string;
  playerType: PlayerType;
  mlbamid?: string | null;
  analysisMode: string;
  updatedAt: string;
};

function isRecentAnalysisEntry(value: unknown): value is RecentAnalysisEntry {
  if (!value || typeof value !== "object") {
    return false;
  }
  const record = value as Record<string, unknown>;
  return (
    typeof record.playerId === "string" &&
    typeof record.playerName === "string" &&
    (record.playerType === "hitter" || record.playerType === "pitcher") &&
    typeof record.analysisMode === "string" &&
    typeof record.updatedAt === "string"
  );
}

export function readRecentAnalyses(): RecentAnalysisEntry[] {
  if (typeof window === "undefined" || !window.localStorage) {
    return [];
  }

  try {
    const raw = window.localStorage.getItem(RECENT_ANALYSES_STORAGE_KEY);
    if (!raw) {
      return [];
    }
    const parsed = JSON.parse(raw);
    if (!Array.isArray(parsed)) {
      return [];
    }
    return parsed
      .filter(isRecentAnalysisEntry)
      .filter((entry) => entry.playerName.trim() !== entry.playerId.trim())
      .sort((a, b) => Date.parse(b.updatedAt) - Date.parse(a.updatedAt))
      .slice(0, MAX_RECENT_ANALYSES);
  } catch (error) {
    console.warn("Unable to parse recent analyses from localStorage", error);
    return [];
  }
}

export function saveRecentAnalysis(
  entry: Omit<RecentAnalysisEntry, "updatedAt">
): RecentAnalysisEntry[] {
  if (typeof window === "undefined" || !window.localStorage) {
    return [];
  }

  const normalized: RecentAnalysisEntry = {
    ...entry,
    updatedAt: new Date().toISOString(),
  };

  const current = readRecentAnalyses();
  const deduped = current.filter(
    (item) => !(item.playerId === normalized.playerId && item.analysisMode === normalized.analysisMode)
  );
  const next = [normalized, ...deduped].slice(0, MAX_RECENT_ANALYSES);

  try {
    window.localStorage.setItem(RECENT_ANALYSES_STORAGE_KEY, JSON.stringify(next));
    window.dispatchEvent(new Event(RECENT_ANALYSES_UPDATED_EVENT));
  } catch (error) {
    console.warn("Unable to write recent analyses to localStorage", error);
  }

  return next;
}

export function getRecentAnalysesUpdatedEventName(): string {
  return RECENT_ANALYSES_UPDATED_EVENT;
}
