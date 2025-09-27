import { useEffect, useMemo, useRef, useState } from "react";
import clsx from "clsx";
import { useMutation, useQuery } from "@tanstack/react-query";
import {
  analyzeComparison,
  analyzePlayer,
  comparePlayers,
  fetchPlayerDetail,
  fetchPlayers,
  fetchVibes,
  registerSession,
} from "../api";
import ReactMarkdown from "react-markdown";
import PlayerStatsCard from "../components/PlayerStatsCard";
import ComparisonTable from "../components/ComparisonTable";
import type {
  AnalysisResponse,
  PlayerDetail,
  PlayerRecord,
  PlayerSummary,
  PlayerType,
} from "../types";
import styles from "../styles/HomePage.module.css";

const SEARCH_MIN_LENGTH = 2;
type Mode = "single" | "compare" | null;

interface SelectedPlayer extends PlayerSummary {}

function HomePage() {
  const [mode, setMode] = useState<Mode>("single");
  const [searchTerm, setSearchTerm] = useState("");
  const [selectedPlayers, setSelectedPlayers] = useState<SelectedPlayer[]>([]);
  const [singleAnalysis, setSingleAnalysis] = useState<AnalysisResponse | null>(null);
  const [compareAnalysis, setCompareAnalysis] = useState<AnalysisResponse | null>(null);
  const [singleVibe, setSingleVibe] = useState<string | null>(null);
  const [compareVibe, setCompareVibe] = useState<string | null>(null);
  const [compareFeedback, setCompareFeedback] = useState<string | null>(null);

  useEffect(() => {
    void registerSession();
  }, []);

  const vibesQuery = useQuery({
    queryKey: ["vibes"],
    queryFn: fetchVibes,
  });

  const defaultVibe = vibesQuery.data?.defaultMode ?? "straightforward";
  const vibes = vibesQuery.data?.vibes ?? [];

  useEffect(() => {
    setSearchTerm("");
    setSelectedPlayers([]);
    setSingleAnalysis(null);
    setCompareAnalysis(null);
    setSingleVibe(null);
    setCompareVibe(null);
    setCompareFeedback(null);
  }, [mode]);

  const trimmedSearch = searchTerm.trim();
  const searchEnabled = trimmedSearch.length >= SEARCH_MIN_LENGTH;

  const searchQuery = useQuery({
    queryKey: ["player-search", trimmedSearch],
    queryFn: async () => {
      if (!searchEnabled) {
        return [] as PlayerSummary[];
      }
      const [hitters, pitchers] = await Promise.all([
        fetchPlayers("hitter", trimmedSearch),
        fetchPlayers("pitcher", trimmedSearch),
      ]);
      const merged = [...hitters, ...pitchers];
      const unique = new Map<string, PlayerSummary>();
      merged.forEach((player) => {
        if (!unique.has(player.id)) {
          unique.set(player.id, player);
        }
      });
      return Array.from(unique.values()).sort((a, b) => a.name.localeCompare(b.name));
    },
    enabled: searchEnabled,
    staleTime: 60_000,
  });

  const suggestions = useMemo(() => {
    if (!searchEnabled) {
      return [] as PlayerSummary[];
    }
    const results = searchQuery.data ?? [];
    const existingIds = new Set(selectedPlayers.map((player) => player.id));
    return results.filter((player) => {
      if (existingIds.has(player.id)) {
        return false;
      }
      if (mode === "compare" && selectedPlayers.length > 0) {
        return player.type === selectedPlayers[0].type;
      }
      return true;
    });
  }, [searchEnabled, searchQuery.data, selectedPlayers, mode]);

  const primaryPlayer = mode === "single" ? selectedPlayers[0] : undefined;
  const compareType: PlayerType | undefined =
    mode === "compare" && selectedPlayers.length > 0 ? selectedPlayers[0].type : undefined;

  const playerDetailQuery = useQuery<PlayerDetail<PlayerRecord>>({
    queryKey: ["player-detail", primaryPlayer?.type, primaryPlayer?.id],
    queryFn: () => fetchPlayerDetail(primaryPlayer!.type, primaryPlayer!.id),
    enabled: mode === "single" && Boolean(primaryPlayer),
  });

  const compareDataQuery = useQuery({
    queryKey: [
      "compare-data",
      compareType,
      selectedPlayers.map((player) => player.id).join("-"),
    ],
    queryFn: () => comparePlayers(compareType!, selectedPlayers.map((player) => player.id)),
    enabled:
      mode === "compare" &&
      Boolean(compareType) &&
      selectedPlayers.length >= 2 &&
      selectedPlayers.every((player) => player.type === compareType),
  });

  const singleAnalysisRequest = useMutation({
    mutationFn: ({ player, vibeId }: { player: SelectedPlayer; vibeId: string }) =>
      analyzePlayer(player.id, player.type, vibeId),
    onSuccess: (data, variables) => {
      setSingleAnalysis(data);
      setSingleVibe(variables.vibeId);
    },
  });

  const compareAnalysisRequest = useMutation({
    mutationFn: ({
      vibeId,
      playerType,
      playerIds,
    }: {
      vibeId: string;
      playerType: PlayerType;
      playerIds: string[];
    }) => analyzeComparison(playerType, playerIds, vibeId),
    onSuccess: (data, variables) => {
      setCompareAnalysis(data);
      setCompareVibe(variables.vibeId);
    },
  });

  const lastSingleRun = useRef<string | null>(null);
  const lastCompareRun = useRef<string | null>(null);

  useEffect(() => {
    if (mode !== "single") {
      lastSingleRun.current = null;
      setSingleAnalysis(null);
      setSingleVibe(null);
      singleAnalysisRequest.reset();
      return;
    }
    const player = primaryPlayer;
    if (!player) {
      lastSingleRun.current = null;
      setSingleAnalysis(null);
      setSingleVibe(null);
      singleAnalysisRequest.reset();
      return;
    }
    if (!defaultVibe || vibesQuery.isLoading) {
      return;
    }
    const key = `${player.id}:${defaultVibe}`;
    if (lastSingleRun.current === key) {
      return;
    }
    lastSingleRun.current = key;
    setSingleVibe(defaultVibe);
    singleAnalysisRequest.mutate({ player, vibeId: defaultVibe });
  }, [mode, primaryPlayer?.id, primaryPlayer?.type, defaultVibe, vibesQuery.isLoading]);

  useEffect(() => {
    if (mode !== "compare") {
      lastCompareRun.current = null;
      setCompareAnalysis(null);
      setCompareVibe(null);
      compareAnalysisRequest.reset();
      return;
    }
    if (!compareType || selectedPlayers.length < 2) {
      lastCompareRun.current = null;
      setCompareAnalysis(null);
      setCompareVibe(null);
      compareAnalysisRequest.reset();
      return;
    }
    if (!defaultVibe || !compareDataQuery.data || vibesQuery.isLoading) {
      return;
    }
    const idsKey = selectedPlayers
      .map((player) => player.id)
      .slice()
      .sort()
      .join(":");
    const key = `${idsKey}:${defaultVibe}`;
    if (lastCompareRun.current === key) {
      return;
    }
    lastCompareRun.current = key;
    setCompareVibe(defaultVibe);
    compareAnalysisRequest.mutate({
      vibeId: defaultVibe,
      playerType: compareType,
      playerIds: selectedPlayers.map((player) => player.id),
    });
  }, [
    mode,
    compareType,
    defaultVibe,
    compareDataQuery.data,
    selectedPlayers,
    vibesQuery.isLoading,
  ]);

  const handleModeChange = (value: Mode) => {
    setMode((current) => (current === value ? current : value));
  };

  const handleSelectPlayer = (player: PlayerSummary) => {
    if (!mode) {
      setMode("single");
    }
    setSearchTerm("");
    if (mode === "compare") {
      if (selectedPlayers.length > 0 && selectedPlayers[0].type !== player.type) {
        setCompareFeedback("Choose hitters or pitchers—mixing them isn't supported yet.");
        return;
      }
      setCompareFeedback(null);
      setSelectedPlayers((current) => {
        if (current.some((item) => item.id === player.id)) {
          return current;
        }
        return [...current, player].slice(0, 4);
      });
    } else {
      setSelectedPlayers([player]);
    }
  };

  const handleRemoveSelected = (playerId: string) => {
    setSelectedPlayers((current) => current.filter((player) => player.id !== playerId));
  };

  const handleSingleVibeChange = (vibeId: string) => {
    if (!primaryPlayer) {
      return;
    }
    setSingleVibe(vibeId);
    lastSingleRun.current = `${primaryPlayer.id}:${vibeId}`;
    singleAnalysisRequest.mutate({ player: primaryPlayer, vibeId });
  };

  const handleCompareVibeChange = (vibeId: string) => {
    if (!compareType || selectedPlayers.length < 2) {
      return;
    }
    setCompareVibe(vibeId);
    const ids = selectedPlayers.map((player) => player.id);
    lastCompareRun.current = `${ids.slice().sort().join(":")}:${vibeId}`;
    compareAnalysisRequest.mutate({ playerType: compareType, playerIds: ids, vibeId });
  };

  return (
    <div className={styles.wrapper}>
      <section className={styles.primer}>
        <div className={styles.modeToggle} role="group" aria-label="Choose analysis mode">
          <button
            type="button"
            className={clsx(styles.modeButton, mode === "single" && styles.modeButtonActive)}
            onClick={() => handleModeChange("single")}
          >
            Analyze one player
          </button>
          <button
            type="button"
            className={clsx(styles.modeButton, mode === "compare" && styles.modeButtonActive)}
            onClick={() => handleModeChange("compare")}
          >
            Compare players
          </button>
        </div>

        <div className={styles.searchSection}>

          <input
            id="player-search"
            type="search"
            className={styles.searchInput}
            placeholder={mode === "compare" ? "Search for hitters or pitchers to compare" : "Search for any hitter or pitcher"}
            value={searchTerm}
            onChange={(event) => setSearchTerm(event.target.value)}
            disabled={false}
          />
          {mode && searchTerm && searchTerm.trim().length < SEARCH_MIN_LENGTH && (
            <p className={styles.helperText}>Keep typing—enter at least two letters.</p>
          )}
          {mode && searchEnabled && suggestions.length === 0 && !searchQuery.isLoading && (
            <p className={styles.helperText}>No matches yet. Try another name.</p>
          )}
          {mode && searchEnabled && suggestions.length > 0 && (
            <ul className={styles.suggestionList}>
              {suggestions.slice(0, 8).map((player) => (
                <li key={player.id}>
                  <button
                    type="button"
                    className={styles.suggestionButton}
                    onClick={() => handleSelectPlayer(player)}
                  >
                    <span>{player.name}</span>
                    <span className={styles.suggestionTag}>{player.type === "hitter" ? "Hitter" : "Pitcher"}</span>
                  </button>
                </li>
              ))}
            </ul>
          )}
        </div>

        {mode === "single" && selectedPlayers[0] && (
          <div className={styles.selectedChips}>
            <button
              type="button"
              className={styles.selectedChip}
              onClick={() => handleRemoveSelected(selectedPlayers[0].id)}
            >
              {selectedPlayers[0].name}
              <span aria-hidden>×</span>
            </button>
          </div>
        )}

        {mode === "compare" && (
          <div className={styles.selectedCompare}>
            <div className={styles.selectedChips}>
              {selectedPlayers.map((player) => (
                <button
                  key={player.id}
                  type="button"
                  className={styles.selectedChip}
                  onClick={() => handleRemoveSelected(player.id)}
                >
                  {player.name}
                  <span aria-hidden>×</span>
                </button>
              ))}
            </div>
            {compareFeedback && <p className={clsx(styles.helperText, styles.feedback)}>{compareFeedback}</p>}
            {selectedPlayers.length < 2 ? (
              <p className={styles.helperText}>Select {2 - selectedPlayers.length} more player{selectedPlayers.length === 1 ? "" : "s"} to compare.</p>
            ) : null}
          </div>
        )}
      </section>

      {mode === "single" && primaryPlayer && playerDetailQuery.data && (
        <section className={styles.singleReveal}>
          <PlayerStatsCard type={primaryPlayer.type} player={playerDetailQuery.data.player} />
          <div className={styles.analysisZone}>
            <div className={styles.analysisHeader}>
              <div>
                <h3>AI scouting report</h3>
                <p>{playerDetailQuery.data.quickInsight}</p>
              </div>
              <div className={styles.vibeChips} role="group" aria-label="Choose analysis vibe">
                {vibes.map((vibe) => (
                  <button
                    key={vibe.id}
                    type="button"
                    className={clsx(styles.vibeChip, singleVibe === vibe.id && styles.vibeChipActive)}
                    onClick={() => handleSingleVibeChange(vibe.id)}
                    disabled={singleAnalysisRequest.isPending || !primaryPlayer}
                  >
                    {vibe.label}
                  </button>
                ))}
              </div>
            </div>
            <div className={styles.analysisBody}>
              {singleAnalysisRequest.isPending && <p className={styles.helperText}>Generating fresh analysis…</p>}
              {singleAnalysis && !singleAnalysisRequest.isPending && (
                <div className={styles.analysisContent}>
                  <ReactMarkdown className={styles.markdown}>{singleAnalysis.analysis}</ReactMarkdown>
                </div>
              )}
            </div>
          </div>
        </section>
      )}

      {mode === "compare" && compareDataQuery.data && selectedPlayers.length >= 2 && (
        <section className={styles.compareReveal}>
          <ComparisonTable
            type={compareType!}
            players={compareDataQuery.data.players}
            recommendedPlayerId={compareDataQuery.data.recommendedPlayerId}
          />
          <div className={styles.analysisZone}>
            <div className={styles.analysisHeader}>
              <div>
                <h3>AI matchup breakdown</h3>
                {compareDataQuery.data.recommendedPlayerId ? (
                  <p>
                    Edge: {compareDataQuery.data.players.find((player) => player.PlayerId === compareDataQuery.data.recommendedPlayerId)?.Name ?? ""}
                  </p>
                ) : (
                  <p>Neck and neck—see how the trends stack up below.</p>
                )}
              </div>
              <div className={styles.vibeChips} role="group" aria-label="Choose analysis vibe">
                {vibes.map((vibe) => (
                  <button
                    key={vibe.id}
                    type="button"
                    className={clsx(styles.vibeChip, compareVibe === vibe.id && styles.vibeChipActive)}
                    onClick={() => handleCompareVibeChange(vibe.id)}
                    disabled={compareAnalysisRequest.isPending || selectedPlayers.length < 2}
                  >
                    {vibe.label}
                  </button>
                ))}
              </div>
            </div>
            <div className={styles.analysisBody}>
              {compareAnalysisRequest.isPending && <p className={styles.helperText}>Crunching the matchup…</p>}
              {compareAnalysis && !compareAnalysisRequest.isPending && (
                <div className={styles.analysisContent}>
                  <ReactMarkdown className={styles.markdown}>{compareAnalysis.analysis}</ReactMarkdown>
                </div>
              )}
            </div>
          </div>
        </section>
      )}
    </div>
  );
}

export default HomePage;
