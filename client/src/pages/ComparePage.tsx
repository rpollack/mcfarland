import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { useMutation, useQuery } from "@tanstack/react-query";
import { analyzeComparison, comparePlayers, fetchPlayers } from "../api";
import { useVibe } from "../contexts/VibeContext";
import type { PlayerSummary, PlayerType } from "../types";
import AnalysisPanel from "../components/AnalysisPanel";
import VibeSelector from "../components/VibeSelector";
import PlayerHeadshot from "../components/PlayerHeadshot";
import styles from "../styles/CompareExperience.module.css";

type ActiveComparison = {
  type: PlayerType;
  ids: string[];
};

type CompareState = {
  playerType: PlayerType;
  playerIds: string[];
};

interface Props {
  initialPlayerType: PlayerType;
  initialPlayerIds: string[];
  onStateChange: (state: CompareState) => void;
}

const MAX_PLAYERS = 3;

function buildAnalysisKey(payload: ActiveComparison & { vibe: string }) {
  return `${payload.type}|${payload.ids.join(",")}|${payload.vibe}`;
}

function hydrateSelectedPlayers(
  current: PlayerSummary[],
  ids: string[],
  playerType: PlayerType,
  fallback?: Map<string, { name: string; mlbamid?: string | null }>
): PlayerSummary[] {
  const existingSummaries = new Map(current.map((player) => [player.id, player]));
  return ids.map((id) => ({
    id,
    name: fallback?.get(id)?.name ?? existingSummaries.get(id)?.name ?? id,
    mlbamid: fallback?.get(id)?.mlbamid ?? existingSummaries.get(id)?.mlbamid,
    type: playerType,
  }));
}

function CompareExperience({ initialPlayerType, initialPlayerIds, onStateChange }: Props) {
  const { mode: vibeMode, vibes } = useVibe();
  const [playerType, setPlayerType] = useState<PlayerType>(initialPlayerType);
  const [searchTerm, setSearchTerm] = useState("");
  const [selectedPlayers, setSelectedPlayers] = useState<PlayerSummary[]>(() =>
    hydrateSelectedPlayers([], initialPlayerIds.slice(0, MAX_PLAYERS), initialPlayerType)
  );
  const [activeComparison, setActiveComparison] = useState<ActiveComparison | null>(null);
  const lastAnalysisKeyRef = useRef<string | null>(null);
  const hasHydratedFromUrl = useRef(false);
  const skipNextAutoCompareRef = useRef(false);

  useEffect(() => {
    setPlayerType(initialPlayerType);
  }, [initialPlayerType]);

  useEffect(() => {
    setSelectedPlayers((current) =>
      hydrateSelectedPlayers(current, initialPlayerIds.slice(0, MAX_PLAYERS), initialPlayerType)
    );
  }, [initialPlayerIds, initialPlayerType]);

  const playersQuery = useQuery({
    queryKey: ["compare-players", playerType, searchTerm],
    queryFn: () => fetchPlayers(playerType, searchTerm),
  });

  const compareMutation = useMutation({
    mutationFn: ({ type, ids }: ActiveComparison) => comparePlayers(type, ids),
  });

  const {
    mutate: runAnalysis,
    reset: resetAnalysis,
    data: analysisData,
    isPending: isAnalysisPending,
  } = useMutation({
    mutationFn: ({ type, ids, vibe }: ActiveComparison & { vibe: string }) =>
      analyzeComparison(type, ids, vibe),
    retry: false,
  });

  const vibeLabel = useMemo(
    () => vibes.find((vibe) => vibe.id === vibeMode)?.label ?? "AI",
    [vibeMode, vibes]
  );

  const triggerAnalysis = useCallback(
    (payload: ActiveComparison & { vibe: string }) => {
      const key = buildAnalysisKey(payload);
      if (lastAnalysisKeyRef.current === key) {
        return;
      }
      lastAnalysisKeyRef.current = key;
      runAnalysis(payload);
    },
    [runAnalysis]
  );

  const clearAnalysisState = useCallback(() => {
    lastAnalysisKeyRef.current = null;
    compareMutation.reset();
    resetAnalysis();
    setActiveComparison(null);
  }, [compareMutation, resetAnalysis]);

  const handleCompare = useCallback(
    async (
      options?: {
        ids?: string[];
        type?: PlayerType;
        updateSelection?: boolean;
      }
    ) => {
      skipNextAutoCompareRef.current = true;
      hasHydratedFromUrl.current = true;
      const ids = (options?.ids ?? selectedPlayers.map((player) => player.id))
        .filter(Boolean)
        .slice(0, MAX_PLAYERS);
      if (ids.length < 2) {
        return;
      }
      const typeToUse = options?.type ?? playerType;
      const payload: ActiveComparison = { type: typeToUse, ids };
      const result = await compareMutation.mutateAsync(payload);
      setActiveComparison(payload);
      lastAnalysisKeyRef.current = null;
      triggerAnalysis({ ...payload, vibe: vibeMode });
      if (options?.updateSelection !== false) {
        const detailsMap = new Map(
          result.players.map((player) => [player.PlayerId, { name: player.Name, mlbamid: player.mlbamid ?? null }])
        );
        setSelectedPlayers((current) => hydrateSelectedPlayers(current, ids, typeToUse, detailsMap));
      }
      return result;
    },
    [selectedPlayers, playerType, compareMutation, triggerAnalysis, vibeMode]
  );

  const handleAddPlayer = useCallback(
    (player: PlayerSummary) => {
      skipNextAutoCompareRef.current = true;
      hasHydratedFromUrl.current = true;
      if (compareMutation.data) {
        clearAnalysisState();
      }
      setSelectedPlayers((current) => {
        if (current.some((entry) => entry.id === player.id) || current.length >= MAX_PLAYERS) {
          return current;
        }
        return [...current, player];
      });
      setSearchTerm("");
      lastAnalysisKeyRef.current = null;
    },
    [clearAnalysisState, compareMutation]
  );

  const handleRemovePlayer = useCallback(
    (id: string) => {
      skipNextAutoCompareRef.current = true;
      hasHydratedFromUrl.current = true;
      if (compareMutation.data) {
        clearAnalysisState();
      }
      setSelectedPlayers((current) => current.filter((player) => player.id !== id));
      lastAnalysisKeyRef.current = null;
    },
    [clearAnalysisState, compareMutation]
  );

  useEffect(() => {
    if (!activeComparison || activeComparison.ids.length < 2) {
      return;
    }
    if (isAnalysisPending) {
      return;
    }
    triggerAnalysis({ ...activeComparison, vibe: vibeMode });
  }, [activeComparison, isAnalysisPending, triggerAnalysis, vibeMode]);

  useEffect(() => {
    onStateChange({
      playerType,
      playerIds: selectedPlayers.map((player) => player.id),
    });
  }, [playerType, selectedPlayers, onStateChange]);

  useEffect(() => {
    const sanitized = initialPlayerIds.slice(0, MAX_PLAYERS);
    if (sanitized.length < 2) {
      hasHydratedFromUrl.current = sanitized.length > 0;
      skipNextAutoCompareRef.current = false;
      return;
    }
    const shouldAutoRunFromUrl = !hasHydratedFromUrl.current && sanitized.length >= 2;

    if (skipNextAutoCompareRef.current && !shouldAutoRunFromUrl) {
      skipNextAutoCompareRef.current = false;
      return;
    }
    skipNextAutoCompareRef.current = false;
    const sameAsActive =
      activeComparison &&
      activeComparison.type === initialPlayerType &&
      activeComparison.ids.length === sanitized.length &&
      activeComparison.ids.every((id, index) => id === sanitized[index]);
    if (sameAsActive && hasHydratedFromUrl.current && !shouldAutoRunFromUrl) {
      return;
    }

    const matchesSelection =
      sanitized.length === selectedPlayers.length &&
      sanitized.every((id, index) => selectedPlayers[index]?.id === id) &&
      playerType === initialPlayerType;

    if (!shouldAutoRunFromUrl && matchesSelection && hasHydratedFromUrl.current) {
      return;
    }
    hasHydratedFromUrl.current = true;
    void handleCompare({ ids: sanitized, type: initialPlayerType });
  }, [
    initialPlayerIds,
    initialPlayerType,
    handleCompare,
    activeComparison,
    playerType,
    selectedPlayers,
  ]);

  const comparisonResult = compareMutation.data;
  const analysisReady = Boolean(analysisData?.analysis);
  const recommendedPlayerId = comparisonResult?.recommendedPlayerId ?? null;

  const availablePlayers = playersQuery.data ?? [];
  const trimmedSearch = searchTerm.trim();
  const canAddMore = selectedPlayers.length < MAX_PLAYERS;
  const hasSelection = selectedPlayers.length > 0;
  const helperMessage = !trimmedSearch
    ? hasSelection
      ? "Search again to add or swap players."
      : "Start typing to add players."
    : null;
  const recommendedPlayerName = recommendedPlayerId
    ? comparisonResult?.players.find((player) => player.PlayerId === recommendedPlayerId)?.Name ?? null
    : null;

  return (
    <div className={styles.container}>
      <section className={styles.searchPanel} aria-label="Comparison search">
        <div className={styles.typeToggle} role="group" aria-label="Player type selector">
          {(["hitter", "pitcher"] as PlayerType[]).map((type) => (
            <button
              key={type}
              type="button"
              onClick={() => {
                setPlayerType(type);
                setSelectedPlayers([]);
                clearAnalysisState();
                setSearchTerm("");
              }}
              className={type === playerType ? styles.activeType : styles.typeButton}
            >
              {type === "hitter" ? "Hitters" : "Pitchers"}
            </button>
          ))}
        </div>

        <input
          id="compare-search"
          type="search"
          value={searchTerm}
          onChange={(event) => setSearchTerm(event.target.value)}
          className={styles.search}
          placeholder={`Find ${playerType === "hitter" ? "hitters" : "pitchers"}...`}
        />

        <ul className={styles.searchResults} aria-live="polite">
          {playersQuery.isLoading && trimmedSearch && <li className={styles.helper}>Loading players…</li>}
          {!playersQuery.isLoading && trimmedSearch && availablePlayers.length === 0 && (
            <li className={styles.helper}>No matches found.</li>
          )}
          {!playersQuery.isLoading &&
            trimmedSearch &&
            availablePlayers.slice(0, 8).map((player) => (
              <li key={player.id}>
                <button
                  type="button"
                  onClick={() => handleAddPlayer(player)}
                  className={styles.resultButton}
                  disabled={!canAddMore || selectedPlayers.some((entry) => entry.id === player.id)}
                >
                  {player.name}
                </button>
              </li>
            ))}
          {!canAddMore && selectedPlayers.length === MAX_PLAYERS && (
            <li className={styles.helper}>You can compare up to three players.</li>
          )}
        </ul>

        {hasSelection && (
          <div className={styles.selectedChips}>
            {selectedPlayers.map((player) => (
              <button
                key={player.id}
                type="button"
                onClick={() => handleRemovePlayer(player.id)}
                className={styles.chip}
              >
                {player.name}
                <span aria-hidden="true">×</span>
              </button>
            ))}
          </div>
        )}

        <button
          type="button"
          className={styles.compareButton}
          onClick={() => {
            void handleCompare();
          }}
          disabled={selectedPlayers.length < 2 || compareMutation.isPending}
        >
          {compareMutation.isPending ? "Comparing…" : "Compare"}
        </button>
      </section>

      {comparisonResult && (
        <div className={styles.results}>
          <section className={styles.summaryCard} aria-label="Selected players">
            <header>
              <h2>Comparison lineup</h2>
              <p>Head-to-head outlook for your {playerType === "hitter" ? "hitters" : "pitchers"}.</p>
            </header>
            <ul className={styles.playerList}>
              {comparisonResult.players.map((player) => {
                const isRecommended = player.PlayerId === recommendedPlayerId;
                return (
                  <li key={player.PlayerId} className={isRecommended ? styles.recommended : undefined}>
                    <div className={styles.playerListItem}>
                      <PlayerHeadshot
                        name={player.Name}
                        playerId={player.PlayerId}
                        mlbamid={player.mlbamid}
                        size={56}
                      />
                      <div className={styles.playerText}>
                        <span className={styles.playerName}>{player.Name}</span>
                      </div>
                      {isRecommended && <span className={styles.badge}>Recommended</span>}
                    </div>
                  </li>
                );
              })}
            </ul>
            <p className={styles.recommendationCopy}>
              {recommendedPlayerName
                ? `${recommendedPlayerName} is McFarland's current pick.`
                : "No clear standout yet—check the AI notes below."}
            </p>
          </section>
          <AnalysisPanel
            quickInsight={
              recommendedPlayerName
                ? `${recommendedPlayerName} projects best right now.`
                : "No clear winner yet."
            }
            isAnalyzing={isAnalysisPending}
            analysis={analysisData?.analysis}
            persona={analysisData?.persona}
            modeLabel={vibeLabel}
          />
          {analysisReady && (
            <div className={styles.vibeSection}>
              <span className={styles.vibeLabel}>Adjust the vibe</span>
              <VibeSelector />
            </div>
          )}
        </div>
      )}
    </div>
  );
}

export default CompareExperience;
