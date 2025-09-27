import { useMemo, useState } from "react";
import { useQuery } from "@tanstack/react-query";
import PlayerPicker from "../components/PlayerPicker";
import PlayerStatsCard from "../components/PlayerStatsCard";
import AnalysisPanel from "../components/AnalysisPanel";
import VibeSelector from "../components/VibeSelector";
import { useVibe } from "../contexts/VibeContext";
import { analyzePlayer, fetchPlayerDetail, fetchPlayers } from "../api";
import type { PlayerType } from "../types";
import styles from "../styles/SingleExperience.module.css";

function SinglePlayerExperience() {
  const { mode, vibes } = useVibe();
  const [playerType, setPlayerType] = useState<PlayerType>("hitter");
  const [searchTerm, setSearchTerm] = useState("");
  const [selectedId, setSelectedId] = useState<string | undefined>();

  const playersQuery = useQuery({
    queryKey: ["players", playerType, searchTerm],
    queryFn: () => fetchPlayers(playerType, searchTerm),
  });

  const detailQuery = useQuery({
    queryKey: ["player-detail", playerType, selectedId],
    queryFn: () => fetchPlayerDetail(playerType, selectedId!),
    enabled: Boolean(selectedId),
  });

  const analysisQuery = useQuery({
    queryKey: ["analysis", "single", playerType, selectedId, mode],
    queryFn: () => analyzePlayer(selectedId!, playerType, mode),
    enabled: Boolean(selectedId),
  });

  const vibeLabel = useMemo(
    () => vibes.find((vibe) => vibe.id === mode)?.label ?? "AI",
    [mode, vibes]
  );

  const hasSelectedPlayer = Boolean(selectedId);
  const hasPlayerProfile = Boolean(detailQuery.data);
  const analysisReady = Boolean(analysisQuery.data?.analysis);

  return (
    <div className={styles.container}>
      <PlayerPicker
        playerType={playerType}
        onTypeChange={(nextType) => {
          setPlayerType(nextType);
          setSelectedId(undefined);
          setSearchTerm("");
        }}
        searchTerm={searchTerm}
        onSearchTermChange={(value) => {
          setSearchTerm(value);
          setSelectedId(undefined);
        }}
        players={playersQuery.data ?? []}
        selectedId={selectedId}
        onSelect={setSelectedId}
        isLoading={playersQuery.isLoading}
      />

      {hasSelectedPlayer && (
        <div className={styles.results}>
          {!hasPlayerProfile || !detailQuery.data ? (
            <div className={styles.loadingCard}>
              <p>{detailQuery.isError ? "We couldn't load that player. Try another search." : "Loading player profile…"}</p>
            </div>
          ) : (
            <>
              <PlayerStatsCard type={playerType} player={detailQuery.data.player} />
              <AnalysisPanel
                quickInsight={detailQuery.data.quickInsight}
                isAnalyzing={analysisQuery.isFetching}
                analysis={analysisQuery.data?.analysis}
                persona={analysisQuery.data?.persona}
                modeLabel={vibeLabel}
              />
              {analysisReady && (
                <div className={styles.vibeSection}>
                  <span className={styles.vibeLabel}>Adjust the vibe</span>
                  <VibeSelector />
                </div>
              )}
            </>
          )}
        </div>
      )}
    </div>
  );
}

export default SinglePlayerExperience;
