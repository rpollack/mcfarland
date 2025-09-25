import type {
  AnalysisResponse,
  CompareResponse,
  PlayerType,
  PlayersResponse,
  StatlineResponse
} from '@/types/api';

const API_BASE_URL = (process.env.NEXT_PUBLIC_API_BASE_URL || 'http://localhost:8000').replace(/\/$/, '');

function buildUrl(path: string) {
  if (path.startsWith('http')) {
    return path;
  }
  return `${API_BASE_URL}${path.startsWith('/') ? path : `/${path}`}`;
}

async function apiFetch<T>(path: string, init?: RequestInit): Promise<T> {
  const response = await fetch(buildUrl(path), {
    ...init,
    headers: {
      Accept: 'application/json',
      'Content-Type': 'application/json',
      ...(init?.headers || {})
    }
  });

  if (!response.ok) {
    const message = await response.text();
    throw new Error(message || `API request failed with status ${response.status}`);
  }

  return (await response.json()) as T;
}

export const swrFetcher = <T>(path: string) => apiFetch<T>(path);

export function fetchPlayers(params?: { type?: PlayerType; search?: string }) {
  const searchParams = new URLSearchParams();
  if (params?.type) {
    searchParams.set('type', params.type);
  }
  if (params?.search) {
    searchParams.set('search', params.search);
  }
  const query = searchParams.toString();
  const path = `/players${query ? `?${query}` : ''}`;
  return apiFetch<PlayersResponse>(path);
}

export function fetchStatline(playerId: string) {
  return apiFetch<StatlineResponse>(`/players/${encodeURIComponent(playerId)}/statline`);
}

export function fetchAnalysis(playerId: string, mode: string) {
  const search = new URLSearchParams({ mode }).toString();
  return apiFetch<AnalysisResponse>(`/players/${encodeURIComponent(playerId)}/analysis?${search}`, {
    cache: 'no-store'
  });
}

export function postCompare(playerIds: string[], mode: string) {
  return apiFetch<CompareResponse>('/compare', {
    method: 'POST',
    body: JSON.stringify({ playerIds, mode }),
    cache: 'no-store'
  });
}

export { apiFetch };
