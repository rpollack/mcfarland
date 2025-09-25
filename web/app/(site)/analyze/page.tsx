'use client';

import { useEffect, useMemo, useState } from 'react';
import { useRouter, useSearchParams } from 'next/navigation';
import useSWR from 'swr';
import clsx from 'clsx';

import PersonaPicker from '@/components/persona-picker';
import StatTable from '@/components/stat-table';
import { PERSONAS } from '@/lib/personas';
import { apiFetch, swrFetcher } from '@/lib/api';
import type {
  AnalysisResponse,
  PlayerSummary,
  PlayerType,
  PlayersResponse,
  StatlineResponse
} from '@/types/api';

const DEFAULT_MODE = 'default';

const TYPE_OPTIONS: Array<{ value: PlayerType; label: string; helper: string }> = [
  { value: 'hitter', label: 'Hitters', helper: 'Everyday bats, utility players, and up-and-comers.' },
  { value: 'pitcher', label: 'Pitchers', helper: 'Starters, bulk arms, and high-leverage relievers.' }
];

function buildQuery(searchParams: URLSearchParams, updates: Record<string, string | null>) {
  const next = new URLSearchParams(searchParams.toString());
  Object.entries(updates).forEach(([key, value]) => {
    if (value === null || value === '') {
      next.delete(key);
    } else {
      next.set(key, value);
    }
  });
  const query = next.toString();
  return query ? `?${query}` : '';
}

export default function AnalyzePage() {
  const router = useRouter();
  const searchParams = useSearchParams();

  const playerType = (searchParams.get('type') === 'pitcher' ? 'pitcher' : 'hitter') as PlayerType;
  const selectedPlayer = searchParams.get('player');
  const analysisMode = searchParams.get('vibe') ?? DEFAULT_MODE;

  const [searchTerm, setSearchTerm] = useState('');

  useEffect(() => {
    setSearchTerm('');
  }, [playerType]);

  const { data: playersData, isLoading: playersLoading, error: playersError } = useSWR<PlayersResponse>(
    `/players?type=${playerType}`,
    swrFetcher
  );

  const players = playersData?.players ?? [];

  const filteredPlayers = useMemo(() => {
    if (!searchTerm) {
      return players;
    }
    const term = searchTerm.toLowerCase();
    return players.filter((player) =>
      [player.displayName, player.name, player.position, player.positionDetail]
        .filter(Boolean)
        .some((value) => value?.toLowerCase().includes(term))
    );
  }, [players, searchTerm]);

  const statlineKey = selectedPlayer ? `/players/${encodeURIComponent(selectedPlayer)}/statline` : null;
  const {
    data: statlineData,
    isLoading: statlineLoading,
    error: statlineError
  } = useSWR<StatlineResponse>(statlineKey, swrFetcher);

  const analysisKey = selectedPlayer
    ? `/players/${encodeURIComponent(selectedPlayer)}/analysis?mode=${encodeURIComponent(analysisMode)}`
    : null;
  const {
    data: analysisData,
    isLoading: analysisLoading,
    error: analysisError,
    mutate: refreshAnalysis
  } = useSWR<AnalysisResponse>(analysisKey, (url: string) => apiFetch<AnalysisResponse>(url, { cache: 'no-store' }));

  const selectedPersona = PERSONAS.find((persona) => persona.mode === analysisMode) ?? PERSONAS[0];

  function handleTypeChange(type: PlayerType) {
    const query = buildQuery(searchParams, {
      type,
      player: null
    });
    router.replace(`/analyze${query}`, { scroll: false });
  }

  function handleSelectPlayer(player: PlayerSummary) {
    const query = buildQuery(searchParams, {
      type: player.type,
      player: player.id
    });
    router.replace(`/analyze${query}`, { scroll: false });
  }

  function handlePersonaChange(mode: string) {
    const query = buildQuery(searchParams, {
      vibe: mode
    });
    router.replace(`/analyze${query}`, { scroll: false });
    if (selectedPlayer) {
      refreshAnalysis();
    }
  }

  const headlinePlayer = statlineData?.player;

  return (
    <div className="space-y-12">
      <section className="overflow-hidden rounded-3xl bg-gradient-to-r from-brand to-brand-dark px-6 py-10 text-white shadow-lg">
        <div className="max-w-3xl space-y-4">
          <p className="text-sm uppercase tracking-[0.4em] text-white/70">Machine-crafted baseball context</p>
          <h1 className="text-3xl font-semibold sm:text-4xl">Pick a player, pick a vibe, get the story.</h1>
          <p className="text-white/90">
            McFARLAND wraps cached FanGraphs data and persona-driven OpenAI prompts in a responsive React
            experience, so your scouting report loads fast and stays connected on every device.
          </p>
        </div>
      </section>

      <section className="grid gap-6 xl:grid-cols-[320px,1fr]">
        <div className="space-y-6">
          <div className="rounded-2xl border border-slate-200 bg-white p-6 shadow-sm">
            <h2 className="text-lg font-semibold text-slate-900">1. Choose a player pool</h2>
            <p className="mt-1 text-sm text-slate-500">Switch between batters and arms to filter the roster.</p>
            <div className="mt-4 grid gap-3">
              {TYPE_OPTIONS.map((option) => (
                <button
                  key={option.value}
                  type="button"
                  onClick={() => handleTypeChange(option.value)}
                  className={clsx(
                    'rounded-xl border border-slate-200 bg-slate-50 px-4 py-3 text-left transition hover:border-brand',
                    option.value === playerType && 'border-brand bg-brand/10'
                  )}
                >
                  <span className="block text-sm font-semibold text-slate-800">{option.label}</span>
                  <span className="mt-1 block text-xs text-slate-500">{option.helper}</span>
                </button>
              ))}
            </div>
          </div>

          <div className="rounded-2xl border border-slate-200 bg-white p-6 shadow-sm">
            <h2 className="text-lg font-semibold text-slate-900">2. Search for a player</h2>
            <input
              type="search"
              value={searchTerm}
              onChange={(event) => setSearchTerm(event.target.value)}
              placeholder="Start typing a name, team, or position"
              className="mt-3 w-full rounded-lg border border-slate-200 px-3 py-2 text-sm focus:border-brand focus:outline-none focus:ring-2 focus:ring-brand/40"
            />
            <div className="mt-4 max-h-96 space-y-2 overflow-y-auto pr-2">
              {playersLoading && <p className="text-sm text-slate-500">Loading roster…</p>}
              {playersError && (
                <p className="text-sm text-rose-600">Unable to load players. Refresh to try again.</p>
              )}
              {!playersLoading && !playersError && filteredPlayers.length === 0 && (
                <p className="text-sm text-slate-500">No players matched that search.</p>
              )}
              {filteredPlayers.map((player) => {
                const isActive = player.id === selectedPlayer;
                return (
                  <button
                    key={player.id}
                    type="button"
                    onClick={() => handleSelectPlayer(player)}
                    className={clsx(
                      'w-full rounded-xl border border-slate-200 bg-white px-4 py-3 text-left text-sm shadow-sm transition hover:border-brand',
                      isActive && 'border-brand bg-brand/10'
                    )}
                  >
                    <div className="flex items-center justify-between">
                      <span className="font-medium text-slate-800">{player.displayName}</span>
                      <span className="text-xs uppercase tracking-wide text-slate-500">{player.sample.label}</span>
                    </div>
                    <div className="mt-1 text-xs text-slate-500">
                      {[player.positionDetail, player.position]
                        .filter((value) => value && value !== player.positionDetail)
                        .join(' • ')}
                    </div>
                  </button>
                );
              })}
            </div>
          </div>
        </div>

        <div className="space-y-6">
          <div className="rounded-2xl border border-slate-200 bg-white p-6 shadow-sm">
            <h2 className="text-lg font-semibold text-slate-900">3. Pick an analysis vibe</h2>
            <p className="mt-1 text-sm text-slate-500">Personas shift the voice without changing the facts.</p>
            <div className="mt-4">
              <PersonaPicker
                personas={PERSONAS}
                selected={analysisMode}
                onSelect={handlePersonaChange}
                disabled={!selectedPlayer}
              />
            </div>
          </div>

          <div className="rounded-2xl border border-slate-200 bg-white p-6 shadow-sm">
            {!selectedPlayer && (
              <div className="py-10 text-center text-slate-500">
                <p className="text-sm">Choose a player to view statlines and AI commentary.</p>
              </div>
            )}

            {selectedPlayer && (
              <div className="space-y-6">
                <header className="flex flex-col gap-3 sm:flex-row sm:items-center sm:justify-between">
                  <div>
                    <h3 className="text-xl font-semibold text-slate-900">
                      {headlinePlayer?.displayName ?? 'Loading player…'}
                    </h3>
                    <p className="text-sm text-slate-500">
                      {headlinePlayer?.type === 'pitcher' ? 'Pitcher' : 'Hitter'} ·{' '}
                      {headlinePlayer?.sample.label}: {headlinePlayer?.sample.value ?? '—'}
                    </p>
                  </div>
                  {headlinePlayer?.photoUrl && (
                    <img
                      src={headlinePlayer.photoUrl}
                      alt={`Headshot of ${headlinePlayer.displayName}`}
                      className="h-16 w-16 rounded-full border border-slate-200 object-cover"
                    />
                  )}
                </header>

                <div>
                  {statlineLoading && <p className="text-sm text-slate-500">Building statline…</p>}
                  {statlineError && (
                    <p className="text-sm text-rose-600">We couldn’t load the statline. Try a different player.</p>
                  )}
                  {statlineData && <StatTable metrics={statlineData.metrics} />}
                </div>

                <section className="space-y-3">
                  <div className="flex items-center justify-between">
                    <h4 className="text-lg font-semibold text-slate-900">
                      {selectedPersona.emoji} {selectedPersona.name} analysis
                    </h4>
                    <button
                      type="button"
                      onClick={() => refreshAnalysis()}
                      className="rounded-full border border-slate-300 px-3 py-1 text-xs font-medium text-slate-600 transition hover:border-brand hover:text-brand"
                    >
                      Refresh
                    </button>
                  </div>
                  {analysisLoading && <p className="text-sm text-slate-500">Calling OpenAI…</p>}
                  {analysisError && (
                    <p className="text-sm text-rose-600">AI analysis failed to load. Try again in a bit.</p>
                  )}
                  {analysisData && (
                    <article
                      className="prose prose-slate max-w-none text-sm"
                      dangerouslySetInnerHTML={{ __html: analysisData.html }}
                    />
                  )}
                </section>
              </div>
            )}
          </div>
        </div>
      </section>
    </div>
  );
}
