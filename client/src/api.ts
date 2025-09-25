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

const API_BASE_URL = import.meta.env.VITE_API_BASE_URL ?? "http://localhost:3000";

async function request<T>(path: string, options?: RequestInit): Promise<T> {
  const response = await fetch(`${API_BASE_URL}${path}`, {
    headers: {
      "Content-Type": "application/json",
    },
    ...options,
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
