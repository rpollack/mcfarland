import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { useMutation, useQuery } from "@tanstack/react-query";
import PlayerPicker from "../components/PlayerPicker";
import AnalysisPanel from "../components/AnalysisPanel";
import VibeSelector from "../components/VibeSelector";
import PlayerHeadshot from "../components/PlayerHeadshot";
import WeeklyTrendsSection from "../components/WeeklyTrendsSection";
import { useVibe } from "../contexts/VibeContext";
import { analyzePlayer, fetchPlayerDetail, fetchPlayers, logShareAnalyticsEvent } from "../api";
import type { PlayerType } from "../types";
import { buildSharePreviewUrl } from "../utils/share";
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
  const [shareStatus, setShareStatus] = useState<"idle" | "success" | "error">("idle");
  const syncingFromPropsRef = useRef(false);

  useEffect(() => {
    syncingFromPropsRef.current = true;
    setPlayerType(initialPlayerType);
    setSelectedId(initialPlayerId);
    if (initialPlayerId) {
      setSearchTerm("");
    }
  }, [initialPlayerType, initialPlayerId]);

  const playersQuery = useQuery({
    queryKey: ["players", playerType, searchTerm],
    queryFn: () => fetchPlayers(playerType, searchTerm),
  });

  const detailQuery = useQuery({
    queryKey: ["player-detail", playerType, selectedId],
    queryFn: () => fetchPlayerDetail(playerType, selectedId!),
    enabled: Boolean(selectedId),
  });

  const lastAnalysisKeyRef = useRef<string | null>(null);
  const {
    mutate: runAnalysis,
    data: analysisData,
    isPending: isAnalysisPending,
  } = useMutation({
    mutationFn: ({ playerId, playerType: type, analysisMode }: { playerId: string; playerType: PlayerType; analysisMode: string }) =>
      analyzePlayer(playerId, type, analysisMode),
    onError: () => {
      lastAnalysisKeyRef.current = null;
    },
    retry: false,
  });

  const vibeLabel = useMemo(
    () => vibes.find((vibe) => vibe.id === mode)?.label ?? "AI",
    [mode, vibes]
  );

  const hasSelectedPlayer = Boolean(selectedId);
  const hasPlayerProfile = Boolean(detailQuery.data);
  const analysisReady = Boolean(analysisData?.analysis);

  useEffect(() => {
    if (!selectedId) {
      lastAnalysisKeyRef.current = null;
      return;
    }

    const key = `${playerType}|${selectedId}|${mode}`;
    if (lastAnalysisKeyRef.current === key) {
      return;
    }
    lastAnalysisKeyRef.current = key;
    runAnalysis({ playerId: selectedId, playerType, analysisMode: mode });
  }, [playerType, selectedId, mode, runAnalysis]);

  useEffect(() => {
    if (syncingFromPropsRef.current) {
      syncingFromPropsRef.current = false;
      return;
    }
    onStateChange({ playerType, playerId: selectedId });
  }, [playerType, selectedId, onStateChange]);

  useEffect(() => {
    if (shareStatus === "idle") {
      return;
    }

    const timer = window.setTimeout(() => setShareStatus("idle"), 2200);
    return () => {
      window.clearTimeout(timer);
    };
  }, [shareStatus]);

  const handleShare = useCallback(async () => {
    if (!detailQuery.data) {
      return;
    }

    try {
      const url = buildSharePreviewUrl(window.location.href);
      await navigator.clipboard.writeText(url);
      setShareStatus("success");
      void logShareAnalyticsEvent({
        playerName: detailQuery.data.player.Name,
        analysisMode: mode,
        eventType: "share_link_copied",
        playerType,
        shareUrl: url,
      });
    } catch (error) {
      console.warn("Failed to copy share link", error);
      setShareStatus("error");
    }
  }, [detailQuery.data, mode, playerType]);

  return (
    <div className={styles.container}>
      <div className={styles.selectionCluster}>
        <PlayerPicker
          embedded
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
        <WeeklyTrendsSection
          embedded
          playerType={playerType}
          onSelectPlayer={(playerId) => {
            setSearchTerm("");
            setSelectedId(playerId);
          }}
        />
      </div>

      {hasSelectedPlayer && (
        <div className={styles.results}>
          {!hasPlayerProfile || !detailQuery.data ? (
            <div className={styles.loadingCard}>
              <p>{detailQuery.isError ? "We couldn't load that player. Try another search." : "Loading player profile…"}</p>
            </div>
          ) : (
            <>
              <AnalysisPanel
                header={(
                  <header className={styles.analysisIdentity}>
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
                )}
                quickInsight={detailQuery.data.quickInsight}
                isAnalyzing={isAnalysisPending}
                analysis={analysisData?.analysis}
                persona={analysisData?.persona}
                modeLabel={vibeLabel}
                actions={
                  analysisReady ? (
                    <div className={styles.analysisActions}>
                      <div className={styles.shareSection}>
                        <button type="button" className={styles.shareButton} onClick={() => void handleShare()}>
                          Share this analysis
                        </button>
                        {shareStatus === "success" && <span className={styles.shareStatus}>Link copied.</span>}
                        {shareStatus === "error" && <span className={styles.shareStatus}>Couldn't copy link.</span>}
                      </div>
                      <div className={styles.vibeSection}>
                        <span className={styles.vibeLabel}>Change the Vibe</span>
                        <VibeSelector />
                      </div>
                    </div>
                  ) : undefined
                }
              />
            </>
          )}
        </div>
      )}
    </div>
  );
}

export default SinglePlayerExperience;
