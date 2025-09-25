'use client';

import { useMemo, useState } from 'react';
import { useRouter, useSearchParams } from 'next/navigation';
import useSWR from 'swr';
import clsx from 'clsx';

import PersonaPicker from '@/components/persona-picker';
import StatTable from '@/components/stat-table';
import { PERSONAS } from '@/lib/personas';
import { postCompare, swrFetcher } from '@/lib/api';
import type {
  CompareResponse,
  PlayerSummary,
  PlayerType,
  PlayersResponse
} from '@/types/api';

const DEFAULT_MODE = 'default';
const MAX_SELECTION = 3;

const TYPE_OPTIONS: Array<{ value: PlayerType; label: string; helper: string }> = [
  { value: 'hitter', label: 'Hitters', helper: 'Pick up to three batters for a vibe check.' },
  { value: 'pitcher', label: 'Pitchers', helper: 'Compare starters or relievers side-by-side.' }
];

function parsePlayers(searchParams: URLSearchParams): string[] {
  const raw = searchParams.get('players');
  if (!raw) {
    return [];
  }
  return raw
    .split(',')
    .map((value) => value.trim())
    .filter(Boolean)
    .slice(0, MAX_SELECTION);
}

function buildQuery(
  searchParams: URLSearchParams,
  updates: { type?: PlayerType | null; vibe?: string | null; players?: string[] }
) {
  const next = new URLSearchParams(searchParams.toString());

  if (typeof updates.type !== 'undefined') {
    if (!updates.type) {
      next.delete('type');
    } else {
      next.set('type', updates.type);
    }
  }

  if (typeof updates.vibe !== 'undefined') {
    if (!updates.vibe) {
      next.delete('vibe');
    } else {
      next.set('vibe', updates.vibe);
    }
  }

  if (typeof updates.players !== 'undefined') {
    if (!updates.players || updates.players.length === 0) {
      next.delete('players');
    } else {
      next.set('players', updates.players.join(','));
    }
  }

  const query = next.toString();
  return query ? `?${query}` : '';
}

export default function ComparePage() {
  const router = useRouter();
  const searchParams = useSearchParams();

  const playerType = (searchParams.get('type') === 'pitcher' ? 'pitcher' : 'hitter') as PlayerType;
  const selectedMode = searchParams.get('vibe') ?? DEFAULT_MODE;
  const selectedPlayers = parsePlayers(searchParams);

  const [searchTerm, setSearchTerm] = useState('');

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
    return players.filter((player) => player.displayName.toLowerCase().includes(term));
  }, [players, searchTerm]);

  const compareKey = selectedPlayers.length
    ? ['/compare', selectedPlayers.join(','), selectedMode]
    : null;

  const {
    data: comparisonData,
    isLoading: comparisonLoading,
    error: comparisonError,
    mutate: refreshComparison
  } = useSWR<CompareResponse>(compareKey, ([, ids, mode]) => postCompare(ids.split(','), mode));

  const selectedPersona = PERSONAS.find((persona) => persona.mode === selectedMode) ?? PERSONAS[0];

  function updateSelection(nextPlayers: string[]) {
    const query = buildQuery(searchParams, { players: nextPlayers, type: playerType });
    router.replace(`/compare${query}`, { scroll: false });
  }

  function handleTogglePlayer(player: PlayerSummary) {
    const isSelected = selectedPlayers.includes(player.id);
    if (isSelected) {
      updateSelection(selectedPlayers.filter((id) => id !== player.id));
      return;
    }

    if (selectedPlayers.length >= MAX_SELECTION) {
      return;
    }

    updateSelection([...selectedPlayers, player.id]);
  }

  function handleTypeChange(type: PlayerType) {
    const query = buildQuery(searchParams, { type, players: [] });
    router.replace(`/compare${query}`, { scroll: false });
  }

  function handlePersonaChange(mode: string) {
    const query = buildQuery(searchParams, { vibe: mode });
    router.replace(`/compare${query}`, { scroll: false });
    if (selectedPlayers.length > 0) {
      refreshComparison();
    }
  }

  return (
    <div className="space-y-12">
      <section className="overflow-hidden rounded-3xl bg-gradient-to-r from-brand-dark to-brand px-6 py-10 text-white shadow-lg">
        <div className="max-w-3xl space-y-4">
          <p className="text-sm uppercase tracking-[0.4em] text-white/70">Who has the edge?</p>
          <h1 className="text-3xl font-semibold sm:text-4xl">Stack up players without losing the vibe.</h1>
          <p className="text-white/90">
            Select up to three players of the same type, switch personas, and let McFARLAND explain who deserves
            the next roster spot.
          </p>
        </div>
      </section>

      <section className="grid gap-6 xl:grid-cols-[320px,1fr]">
        <div className="space-y-6">
          <div className="rounded-2xl border border-slate-200 bg-white p-6 shadow-sm">
            <h2 className="text-lg font-semibold text-slate-900">1. Choose hitter or pitcher mode</h2>
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
            <h2 className="text-lg font-semibold text-slate-900">2. Add up to three players</h2>
            <p className="mt-1 text-sm text-slate-500">Selected players stay highlighted in the list.</p>
            <input
              type="search"
              value={searchTerm}
              onChange={(event) => setSearchTerm(event.target.value)}
              placeholder="Filter by player name"
              className="mt-3 w-full rounded-lg border border-slate-200 px-3 py-2 text-sm focus:border-brand focus:outline-none focus:ring-2 focus:ring-brand/40"
            />
            <div className="mt-4 max-h-96 space-y-2 overflow-y-auto pr-2">
              {playersLoading && <p className="text-sm text-slate-500">Loading roster…</p>}
              {playersError && (
                <p className="text-sm text-rose-600">Unable to load players. Refresh to try again.</p>
              )}
              {!playersLoading && !playersError && filteredPlayers.length === 0 && (
                <p className="text-sm text-slate-500">No matches for that search.</p>
              )}
              {filteredPlayers.map((player) => {
                const isSelected = selectedPlayers.includes(player.id);
                const disabled = !isSelected && selectedPlayers.length >= MAX_SELECTION;
                return (
                  <button
                    key={player.id}
                    type="button"
                    onClick={() => handleTogglePlayer(player)}
                    disabled={disabled}
                    className={clsx(
                      'w-full rounded-xl border border-slate-200 bg-white px-4 py-3 text-left text-sm shadow-sm transition hover:border-brand',
                      isSelected && 'border-brand bg-brand/10',
                      disabled && 'cursor-not-allowed opacity-50 hover:border-slate-200'
                    )}
                  >
                    <div className="flex items-center justify-between">
                      <span className="font-medium text-slate-800">{player.displayName}</span>
                      <span className="text-xs uppercase tracking-wide text-slate-500">{player.sample.label}</span>
                    </div>
                    <div className="mt-1 text-xs text-slate-500">{player.positionDetail ?? player.position ?? ''}</div>
                  </button>
                );
              })}
            </div>
          </div>
        </div>

        <div className="space-y-6">
          <div className="rounded-2xl border border-slate-200 bg-white p-6 shadow-sm">
            <h2 className="text-lg font-semibold text-slate-900">3. Select a persona</h2>
            <p className="mt-1 text-sm text-slate-500">Shift the tone while comparing the same data.</p>
            <div className="mt-4">
              <PersonaPicker
                personas={PERSONAS}
                selected={selectedMode}
                onSelect={handlePersonaChange}
                disabled={selectedPlayers.length === 0}
              />
            </div>
          </div>

          <div className="rounded-2xl border border-slate-200 bg-white p-6 shadow-sm">
            {selectedPlayers.length === 0 && (
              <div className="py-10 text-center text-slate-500">
                <p className="text-sm">Add players to unlock stat tables and recommendations.</p>
              </div>
            )}

            {selectedPlayers.length > 0 && (
              <div className="space-y-6">
                {comparisonLoading && <p className="text-sm text-slate-500">Crunching the numbers…</p>}
                {comparisonError && (
                  <p className="text-sm text-rose-600">Comparison failed to load. Please try again.</p>
                )}
                {comparisonData && (
                  <div className="space-y-6">
                    {comparisonData.recommended && (
                      <div className="rounded-xl bg-brand/10 p-4 text-sm text-brand-dark">
                        <p className="font-semibold">
                          Recommendation: {comparisonData.recommended.displayName} has the edge right now.
                        </p>
                      </div>
                    )}

                    <div className="grid gap-6 lg:grid-cols-2 xl:grid-cols-3">
                      {comparisonData.players.map((entry) => (
                        <div key={entry.player.id} className="space-y-4">
                          <div className="flex items-center justify-between">
                            <h3 className="text-base font-semibold text-slate-900">{entry.player.displayName}</h3>
                            <button
                              type="button"
                              onClick={() => updateSelection(selectedPlayers.filter((id) => id !== entry.player.id))}
                              className="text-xs font-medium text-rose-600 hover:underline"
                            >
                              Remove
                            </button>
                          </div>
                          <StatTable metrics={entry.metrics} />
                        </div>
                      ))}
                    </div>

                    <section className="space-y-3">
                      <div className="flex items-center justify-between">
                        <h4 className="text-lg font-semibold text-slate-900">
                          {selectedPersona.emoji} {selectedPersona.name} analysis
                        </h4>
                        <button
                          type="button"
                          onClick={() => refreshComparison()}
                          className="rounded-full border border-slate-300 px-3 py-1 text-xs font-medium text-slate-600 transition hover:border-brand hover:text-brand"
                        >
                          Refresh
                        </button>
                      </div>
                      <article
                        className="prose prose-slate max-w-none text-sm"
                        dangerouslySetInnerHTML={{ __html: comparisonData.analysisHtml }}
                      />
                    </section>
                  </div>
                )}
              </div>
            )}
          </div>
        </div>
      </section>
    </div>
  );
}
