import { useEffect, useMemo, useState } from "react";
import { useMutation, useQuery } from "@tanstack/react-query";
import { analyzeComparison, comparePlayers, fetchPlayers } from "../api";
import { useVibe } from "../contexts/VibeContext";
import type { PlayerSummary, PlayerType } from "../types";
import ComparisonTable from "../components/ComparisonTable";
import AnalysisPanel from "../components/AnalysisPanel";
import styles from "../styles/ComparePage.module.css";

function ComparePage() {
  const { mode, vibes } = useVibe();
  const [playerType, setPlayerType] = useState<PlayerType>("hitter");
  const [searchA, setSearchA] = useState("");
  const [searchB, setSearchB] = useState("");
  const [playerA, setPlayerA] = useState<string | undefined>(undefined);
  const [playerB, setPlayerB] = useState<string | undefined>(undefined);

  const playersQueryA = useQuery({
    queryKey: ["compare-players", playerType, "A", searchA],
    queryFn: () => fetchPlayers(playerType, searchA),
  });

  const playersQueryB = useQuery({
    queryKey: ["compare-players", playerType, "B", searchB],
    queryFn: () => fetchPlayers(playerType, searchB),
  });

  const comparisonQuery = useQuery({
    queryKey: ["comparison", playerType, playerA, playerB],
    queryFn: () => comparePlayers(playerType, [playerA!, playerB!]),
    enabled: Boolean(playerA && playerB),
  });

  const analysisMutation = useMutation({
    mutationFn: () => analyzeComparison(playerType, [playerA!, playerB!], mode),
  });

  useEffect(() => {
    analysisMutation.reset();
  }, [playerA, playerB, playerType, mode]);

  const selectedVibe = useMemo(() => vibes.find((vibe) => vibe.id === mode)?.label ?? "AI", [mode, vibes]);

  const players: PlayerSummary[] = playersQueryA.data ?? [];
  const opponentPlayers: PlayerSummary[] = playersQueryB.data ?? [];

  const recommendedPlayerId = comparisonQuery.data?.recommendedPlayerId ?? null;

  return (
    <div className={styles.layout}>
      <section className={styles.selectorPanel} aria-label="Comparison setup">
        <div className={styles.typeToggle} role="group" aria-label="Player type selector">
          {(["hitter", "pitcher"] as PlayerType[]).map((type) => (
            <button
              key={type}
              type="button"
              onClick={() => {
                setPlayerType(type);
                setPlayerA(undefined);
                setPlayerB(undefined);
                setSearchA("");
                setSearchB("");
              }}
              className={type === playerType ? styles.activeType : styles.typeButton}
            >
              {type === "hitter" ? "Hitters" : "Pitchers"}
            </button>
          ))}
        </div>

        <PlayerSelect
          id="player-a"
          label="Player A"
          players={players}
          isLoading={playersQueryA.isLoading}
          search={searchA}
          onSearch={setSearchA}
          selectedId={playerA}
          onSelect={setPlayerA}
        />

        <PlayerSelect
          id="player-b"
          label="Player B"
          players={opponentPlayers}
          isLoading={playersQueryB.isLoading}
          search={searchB}
          onSearch={setSearchB}
          selectedId={playerB}
          onSelect={setPlayerB}
        />
      </section>

      <div className={styles.content}>
        {!comparisonQuery.data ? (
          <div className={styles.placeholder}>
            <h2>Select two {playerType === "hitter" ? "hitters" : "pitchers"}</h2>
            <p>We will highlight the stronger profile based on expected production (xwOBA for hitters, xERA for pitchers).</p>
          </div>
        ) : (
          <>
            <ComparisonTable
              type={playerType}
              players={comparisonQuery.data.players}
              recommendedPlayerId={recommendedPlayerId}
            />
            <AnalysisPanel
              quickInsight={recommendedPlayerId ? `${comparisonQuery.data.players.find((p) => p.PlayerId === recommendedPlayerId)?.Name} projects best right now.` : "No clear winner yet."}
              onAnalyze={() => analysisMutation.mutate()}
              isAnalyzing={analysisMutation.isLoading}
              analysis={analysisMutation.data?.analysis}
              persona={analysisMutation.data?.persona}
              disabled={!playerA || !playerB || comparisonQuery.isLoading}
              modeLabel={selectedVibe}
            />
          </>
        )}
      </div>
    </div>
  );
}

interface PlayerSelectProps {
  id: string;
  label: string;
  players: PlayerSummary[];
  isLoading: boolean;
  search: string;
  onSearch: (term: string) => void;
  selectedId?: string;
  onSelect: (playerId: string | undefined) => void;
}

function PlayerSelect({ id, label, players, isLoading, search, onSearch, selectedId, onSelect }: PlayerSelectProps) {
  return (
    <div className={styles.playerSelect}>
      <label htmlFor={`${id}-search`} className={styles.label}>
        {label} search
      </label>
      <input
        id={`${id}-search`}
        type="search"
        value={search}
        onChange={(event) => onSearch(event.target.value)}
        className={styles.search}
        placeholder={`Search for ${label.toLowerCase()}`}
      />
      <label htmlFor={`${id}-select`} className={styles.label}>
        {label}
      </label>
      <select
        id={`${id}-select`}
        value={selectedId ?? ""}
        onChange={(event) => onSelect(event.target.value || undefined)}
        className={styles.select}
      >
        <option value="">Select a player</option>
        {isLoading ? (
          <option>Loading...</option>
        ) : (
          players.map((player) => (
            <option key={player.id} value={player.id}>
              {player.name}
            </option>
          ))
        )}
      </select>
    </div>
  );
}

export default ComparePage;
