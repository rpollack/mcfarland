import { useEffect, useMemo, useState } from "react";
import { useMutation, useQuery } from "@tanstack/react-query";
import PlayerPicker from "../components/PlayerPicker";
import PlayerStatsCard from "../components/PlayerStatsCard";
import AnalysisPanel from "../components/AnalysisPanel";
import { useVibe } from "../contexts/VibeContext";
import { analyzePlayer, fetchPlayerDetail, fetchPlayers } from "../api";
import type { PlayerType } from "../types";
import styles from "../styles/SinglePlayerPage.module.css";

function SinglePlayerPage() {
  const { mode, vibes } = useVibe();
  const [playerType, setPlayerType] = useState<PlayerType>("hitter");
  const [searchTerm, setSearchTerm] = useState("");
  const [selectedId, setSelectedId] = useState<string | undefined>(undefined);

  const playersQuery = useQuery({
    queryKey: ["players", playerType, searchTerm],
    queryFn: () => fetchPlayers(playerType, searchTerm),
  });

  useEffect(() => {
    if (playersQuery.data && playersQuery.data.length > 0 && !selectedId) {
      setSelectedId(playersQuery.data[0].id);
    }
  }, [playersQuery.data, selectedId]);

  useEffect(() => {
    setSelectedId(undefined);
  }, [playerType]);

  const detailQuery = useQuery({
    queryKey: ["player-detail", playerType, selectedId],
    queryFn: () => fetchPlayerDetail(playerType, selectedId!),
    enabled: Boolean(selectedId),
  });

  const analysisMutation = useMutation({
    mutationFn: () => analyzePlayer(selectedId!, playerType, mode),
  });

  useEffect(() => {
    analysisMutation.reset();
  }, [selectedId, playerType, mode]);

  const selectedVibe = useMemo(() => vibes.find((vibe) => vibe.id === mode)?.label ?? "AI", [mode, vibes]);

  return (
    <div className={styles.layout}>
      <PlayerPicker
        playerType={playerType}
        onTypeChange={setPlayerType}
        searchTerm={searchTerm}
        onSearchTermChange={setSearchTerm}
        players={playersQuery.data ?? []}
        selectedId={selectedId}
        onSelect={setSelectedId}
        isLoading={playersQuery.isLoading}
      />
      <div className={styles.content}>
        {!selectedId || !detailQuery.data ? (
          <div className={styles.placeholder}>
            <h2>Select a player to begin</h2>
            <p>Choose a hitter or pitcher to see full stat trends and request an AI scouting report.</p>
          </div>
        ) : (
          <>
            <PlayerStatsCard type={playerType} player={detailQuery.data.player} />
            <AnalysisPanel
              quickInsight={detailQuery.data.quickInsight}
              onAnalyze={() => analysisMutation.mutate()}
              isAnalyzing={analysisMutation.isLoading}
              analysis={analysisMutation.data?.analysis}
              persona={analysisMutation.data?.persona}
              disabled={!selectedId || detailQuery.isLoading}
              modeLabel={selectedVibe}
            />
          </>
        )}
      </div>
    </div>
  );
}

export default SinglePlayerPage;
