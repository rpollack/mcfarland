import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { useMutation, useQuery } from "@tanstack/react-query";
import { analyzeComparison, comparePlayers, fetchPlayers } from "../api";
import { useVibe } from "../contexts/VibeContext";
import type { PlayerSummary, PlayerType } from "../types";
import AnalysisPanel from "../components/AnalysisPanel";
import VibeSelector from "../components/VibeSelector";
import styles from "../styles/CompareExperience.module.css";

type ActiveComparison = {
  type: PlayerType;
  ids: string[];
};

function CompareExperience() {
  const { mode, vibes } = useVibe();
  const [playerType, setPlayerType] = useState<PlayerType>("hitter");
  const [searchTerm, setSearchTerm] = useState("");
  const [selectedPlayers, setSelectedPlayers] = useState<PlayerSummary[]>([]);
  const [activeComparison, setActiveComparison] = useState<ActiveComparison | null>(null);
  const lastAnalysisKeyRef = useRef<string | null>(null);

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
  });

  const vibeLabel = useMemo(
    () => vibes.find((vibe) => vibe.id === mode)?.label ?? "AI",
    [mode, vibes]
  );

  const makeAnalysisKey = (payload: ActiveComparison & { vibe: string }) =>
    `${payload.type}|${payload.ids.join(",")}|${payload.vibe}`;

  const triggerAnalysis = useCallback((payload: ActiveComparison & { vibe: string }) => {
    const key = makeAnalysisKey(payload);
    if (lastAnalysisKeyRef.current === key) {
      return;
    }
    lastAnalysisKeyRef.current = key;
    runAnalysis(payload);
  }, [runAnalysis]);

  const clearAnalysisState = () => {
    lastAnalysisKeyRef.current = null;
    compareMutation.reset();
    resetAnalysis();
    setActiveComparison(null);
  };

  const handleAddPlayer = (player: PlayerSummary) => {
    if (compareMutation.data) {
      clearAnalysisState();
    }

    setSelectedPlayers((current) => {
      if (current.some((entry) => entry.id === player.id) || current.length >= 3) {
        return current;
      }
      return [...current, player];
    });
    setSearchTerm("");
    lastAnalysisKeyRef.current = null;
  };

  const handleRemovePlayer = (id: string) => {
    if (compareMutation.data) {
      clearAnalysisState();
    }

    setSelectedPlayers((current) => current.filter((player) => player.id !== id));
    lastAnalysisKeyRef.current = null;
  };

  const handleCompare = async () => {
    const ids = selectedPlayers.map((player) => player.id);
    if (ids.length < 2) {
      return;
    }

    const payload: ActiveComparison = { type: playerType, ids };
    const result = await compareMutation.mutateAsync(payload);
    setActiveComparison(payload);
    lastAnalysisKeyRef.current = null;
    triggerAnalysis({ ...payload, vibe: mode });
    return result;
  };

  useEffect(() => {
    if (!activeComparison || activeComparison.ids.length < 2) {
      return;
    }

    if (isAnalysisPending) {
      return;
    }

    triggerAnalysis({ ...activeComparison, vibe: mode });
  }, [mode, activeComparison, isAnalysisPending, triggerAnalysis]);

  const comparisonResult = compareMutation.data;
  const analysisReady = Boolean(analysisData?.analysis);
  const recommendedPlayerId = comparisonResult?.recommendedPlayerId ?? null;

  const availablePlayers = playersQuery.data ?? [];
  const trimmedSearch = searchTerm.trim();
  const canAddMore = selectedPlayers.length < 3;
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

        <label htmlFor="compare-search" className={styles.label}>
          Search players
        </label>
        <input
          id="compare-search"
          type="search"
          value={searchTerm}
          onChange={(event) => setSearchTerm(event.target.value)}
          className={styles.search}
          placeholder={`Find ${playerType === "hitter" ? "hitters" : "pitchers"}`}
        />

        <ul className={styles.searchResults} aria-live="polite">
          {helperMessage && <li className={styles.helper}>{helperMessage}</li>}
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
          {!canAddMore && selectedPlayers.length === 3 && (
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
          onClick={handleCompare}
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
                    <span className={styles.playerName}>{player.Name}</span>
                    {isRecommended && <span className={styles.badge}>Recommended</span>}
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
