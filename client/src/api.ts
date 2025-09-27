import {
  AboutContent,
  AnalysisResponse,
  ComparisonResponse,
  HitterRecord,
  PitcherRecord,
  PlayerDetail,
  PlayerRecord,
  PlayerSummary,
  PlayerType,
  Vibe,
} from "./types";

const API_BASE_URL =
  import.meta.env.VITE_API_BASE_URL ??
  (typeof window !== "undefined" ? "" : "http://localhost:3000");

function buildUrl(path: string): string {
  if (!API_BASE_URL) {
    return path;
  }

  const base = API_BASE_URL.endsWith("/") ? API_BASE_URL.slice(0, -1) : API_BASE_URL;
  return `${base}${path}`;
}

async function request<T>(path: string, options?: RequestInit): Promise<T> {
  const sessionId = getOrCreateSessionId();
  const headers = new Headers({ "Content-Type": "application/json" });

  if (sessionId) {
    headers.set("X-Session-Id", sessionId);
  }

  if (options?.headers) {
    new Headers(options.headers).forEach((value, key) => {
      headers.set(key, value);
    });
  }

  const response = await fetch(buildUrl(path), {
    ...options,
    headers,
  });

  if (!response.ok) {
    const errorBody = await response.json().catch(() => ({}));
    const message = errorBody.error ?? response.statusText;
    throw new Error(message);
  }

  return (await response.json()) as T;
}

export async function fetchPlayers(type: PlayerType, query?: string): Promise<PlayerSummary[]> {
  const params = new URLSearchParams({ type });
  if (query) {
    params.set("q", query);
  }
  const data = await request<{ players: PlayerSummary[] }>(`/api/players?${params.toString()}`);
  return data.players;
}

export async function fetchPlayerDetail<T extends PlayerRecord>(type: PlayerType, playerId: string): Promise<PlayerDetail<T>> {
  const params = new URLSearchParams({ type });
  return request<PlayerDetail<T>>(`/api/players/${playerId}?${params.toString()}`);
}

export async function analyzePlayer(playerId: string, playerType: PlayerType, analysisMode: string): Promise<AnalysisResponse> {
  return request<AnalysisResponse>(`/api/analyze`, {
    method: "POST",
    body: JSON.stringify({ playerId, playerType, analysisMode }),
  });
}

export async function comparePlayers<T extends PlayerRecord>(playerType: PlayerType, playerIds: string[]): Promise<ComparisonResponse<T>> {
  return request<ComparisonResponse<T>>(`/api/compare`, {
    method: "POST",
    body: JSON.stringify({ playerType, playerIds }),
  });
}

export async function analyzeComparison(playerType: PlayerType, playerIds: string[], analysisMode: string): Promise<AnalysisResponse> {
  return request<AnalysisResponse>(`/api/compare/analyze`, {
    method: "POST",
    body: JSON.stringify({ playerType, playerIds, analysisMode }),
  });
}

export async function fetchVibes(): Promise<{ vibes: Vibe[]; defaultMode: string }> {
  return request<{ vibes: Vibe[]; defaultMode: string }>(`/api/vibes`);
}

export async function fetchAbout(): Promise<AboutContent> {
  return request<AboutContent>(`/api/about`);
}

export type { HitterRecord, PitcherRecord };

const SESSION_STORAGE_KEY = "mcfarland-session-id";

function getOrCreateSessionId(): string | null {
  if (typeof window === "undefined" || !window.localStorage) {
    return null;
  }

  try {
    let sessionId = window.localStorage.getItem(SESSION_STORAGE_KEY);
    if (!sessionId) {
      if (typeof crypto !== "undefined" && "randomUUID" in crypto) {
        sessionId = crypto.randomUUID();
      } else {
        sessionId = `${Date.now()}-${Math.random().toString(36).slice(2)}`;
      }
      window.localStorage.setItem(SESSION_STORAGE_KEY, sessionId);
    }
    return sessionId;
  } catch (error) {
    console.warn("Unable to access localStorage for session tracking", error);
    return null;
  }
}

export async function registerSession(): Promise<void> {
  const sessionId = getOrCreateSessionId();
  if (!sessionId) {
    return;
  }

  try {
    await fetch(buildUrl("/api/sessions"), {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify({ sessionId }),
    });
  } catch (error) {
    console.warn("Failed to register analytics session", error);
  }
}

export async function logShareAnalyticsEvent({
  playerName,
  analysisMode,
  eventType,
  playerType,
  shareUrl,
}: {
  playerName: string;
  analysisMode: string;
  eventType: string;
  playerType?: PlayerType;
  shareUrl?: string;
}): Promise<void> {
  const sessionId = getOrCreateSessionId();
  if (!sessionId) {
    return;
  }

  try {
    await fetch(buildUrl("/api/share-events"), {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify({
        sessionId,
        playerName,
        analysisMode,
        eventType,
        playerType,
        shareUrl,
      }),
    });
  } catch (error) {
    console.warn("Failed to log share analytics event", error);
  }
}
