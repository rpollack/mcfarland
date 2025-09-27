import { useEffect, useMemo, useState } from "react";
import { useQuery } from "@tanstack/react-query";
import PlayerPicker from "../components/PlayerPicker";
import AnalysisPanel from "../components/AnalysisPanel";
import VibeSelector from "../components/VibeSelector";
import PlayerHeadshot from "../components/PlayerHeadshot";
import { useVibe } from "../contexts/VibeContext";
import { analyzePlayer, fetchPlayerDetail, fetchPlayers } from "../api";
import type { PlayerType } from "../types";
import styles from "../styles/SingleExperience.module.css";

interface Props {
  initialPlayerType: PlayerType;
  initialPlayerId?: string;
  onStateChange: (state: { playerType: PlayerType; playerId?: string }) => void;
}

function SinglePlayerExperience({ initialPlayerType, initialPlayerId, onStateChange }: Props) {
  const { mode, vibes } = useVibe();
  const [playerType, setPlayerType] = useState<PlayerType>(initialPlayerType);
  const [searchTerm, setSearchTerm] = useState("");
  const [selectedId, setSelectedId] = useState<string | undefined>(initialPlayerId);

  useEffect(() => {
    setPlayerType(initialPlayerType);
  }, [initialPlayerType]);

  useEffect(() => {
    setSelectedId(initialPlayerId);
    if (initialPlayerId) {
      setSearchTerm("");
    }
  }, [initialPlayerId]);

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

  useEffect(() => {
    onStateChange({ playerType, playerId: selectedId });
  }, [playerType, selectedId, onStateChange]);

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
          if (value.trim().length > 0) {
            setSelectedId(undefined);
          }
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
              <header className={styles.playerHeader}>
                <div className={styles.playerHeaderContent}>
                  <PlayerHeadshot
                    name={detailQuery.data.player.Name}
                    playerId={detailQuery.data.player.PlayerId}
                    mlbamid={detailQuery.data.player.mlbamid}
                    size={72}
                  />
                  <div className={styles.playerMeta}>
                    <h2>{detailQuery.data.player.Name}</h2>
                    <p>Latest {playerType === "hitter" ? "hitter" : "pitcher"} outlook from McFarland AI.</p>
                  </div>
                </div>
              </header>
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
