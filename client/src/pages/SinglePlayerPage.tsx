import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { useMutation, useQuery } from "@tanstack/react-query";
import PlayerPicker from "../components/PlayerPicker";
import AnalysisPanel from "../components/AnalysisPanel";
import VibeSelector from "../components/VibeSelector";
import PlayerHeadshot from "../components/PlayerHeadshot";
import WeeklyTrendsSection from "../components/WeeklyTrendsSection";
import panelStyles from "../styles/AnalysisPanel.module.css";
import { useVibe } from "../contexts/VibeContext";
import {
  analyzePlayer,
  fetchDataFreshness,
  fetchPlayerDetail,
  fetchPlayers,
  logShareAnalyticsEvent,
  trackAnalysisRunAnalyticsEvent,
} from "../api";
import type { HitterRecord, PitcherRecord, PlayerType } from "../types";
import { buildSharePreviewUrl } from "../utils/share";
import { saveRecentAnalysis } from "../utils/recentAnalyses";
import styles from "../styles/SingleExperience.module.css";

interface Props {
  initialPlayerType: PlayerType;
  initialPlayerId?: string;
  onStateChange: (state: { playerType: PlayerType; playerId?: string }) => void;
}

function formatSlashStat(value: number | null | undefined): string {
  if (value === null || value === undefined || Number.isNaN(value)) {
    return "N/A";
  }
  return Number(value).toFixed(3).replace(/^(-?)0+/, "$1");
}

function formatInteger(value: number | null | undefined): string {
  if (value === null || value === undefined || Number.isNaN(value)) {
    return "N/A";
  }
  return String(Math.round(value));
}

function formatEra(value: number | null | undefined): string {
  if (value === null || value === undefined || Number.isNaN(value)) {
    return "N/A";
  }
  return Number(value).toFixed(2);
}

function buildQuickStatsLine(
  player: HitterRecord | PitcherRecord,
  playerType: PlayerType,
  dataThroughLabel?: string
): string | undefined {
  if (!dataThroughLabel) {
    return undefined;
  }

  if (playerType === "hitter") {
    const hitter = player as HitterRecord;
    return `Through ${dataThroughLabel}: ${formatInteger(hitter.PA_cur)} PA · ${formatSlashStat(hitter.AVG_cur)}/${formatSlashStat(hitter.OBP_cur)}/${formatSlashStat(hitter.SLG_cur)}`;
  }

  const pitcher = player as PitcherRecord;
  return `Through ${dataThroughLabel}: ${formatInteger(pitcher.tbf)} TBF · ${formatEra(pitcher.era_cur)} ERA · ${formatInteger(pitcher.so)} SO · ${formatInteger(pitcher.bb)} BB`;
}

function SinglePlayerExperience({ initialPlayerType, initialPlayerId, onStateChange }: Props) {
  const { mode, setMode, vibes } = useVibe();
  const [playerType, setPlayerType] = useState<PlayerType>(initialPlayerType);
  const [searchTerm, setSearchTerm] = useState("");
  const [selectedId, setSelectedId] = useState<string | undefined>(initialPlayerId);
  const [selectionSource, setSelectionSource] = useState<"recent_analyze_again" | "main_ui">("main_ui");
  const [shareStatus, setShareStatus] = useState<"idle" | "success" | "error">("idle");
  const [isVibeChooserOpen, setIsVibeChooserOpen] = useState(false);
  const syncingFromPropsRef = useRef(false);

  useEffect(() => {
    syncingFromPropsRef.current = true;
    setPlayerType(initialPlayerType);
    setSelectedId(initialPlayerId);
    setIsVibeChooserOpen(false);
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

  const freshnessQuery = useQuery({
    queryKey: ["data-freshness"],
    queryFn: fetchDataFreshness,
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
  const quickStatsLine =
    detailQuery.data && freshnessQuery.data
      ? buildQuickStatsLine(detailQuery.data.player, playerType, freshnessQuery.data.dataThroughLabel)
      : undefined;

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

  const lastPersistedAnalysisRef = useRef<string | null>(null);

  useEffect(() => {
    if (!selectedId || !analysisData?.analysis || !detailQuery.data?.player) {
      return;
    }

    const key = `${playerType}|${selectedId}|${mode}|${analysisData.prompt}`;
    if (lastPersistedAnalysisRef.current === key) {
      return;
    }
    lastPersistedAnalysisRef.current = key;

    saveRecentAnalysis({
      playerId: selectedId,
      playerName: detailQuery.data.player.Name,
      playerType,
      mlbamid: detailQuery.data.player.mlbamid ?? null,
      analysisMode: mode,
    });

    trackAnalysisRunAnalyticsEvent({
      playerId: selectedId,
      playerName: detailQuery.data.player.Name,
      playerType,
      analysisMode: mode,
      selectionSource,
    });
  }, [analysisData?.analysis, analysisData?.prompt, detailQuery.data, mode, playerType, selectedId, selectionSource]);

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

  const handleAnalyzeAnother = useCallback(() => {
    setSelectedId(undefined);
    setSearchTerm("");
    setShareStatus("idle");
    setIsVibeChooserOpen(false);
    lastAnalysisKeyRef.current = null;
  }, []);

  return (
    <div className={styles.container}>
      <div className={styles.selectionCluster}>
        <PlayerPicker
          embedded
          playerType={playerType}
          onTypeChange={(nextType) => {
            setPlayerType(nextType);
            setSelectedId(undefined);
            setSelectionSource("main_ui");
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
          onSelect={(playerId) => {
            setSelectionSource("main_ui");
            setSelectedId(playerId);
          }}
          isLoading={playersQuery.isLoading}
        />
        <WeeklyTrendsSection
          embedded
          playerType={playerType}
          onSelectPlayer={({ playerId, source, analysisMode }) => {
            setSelectionSource(source);
            if (analysisMode && analysisMode !== mode) {
              setMode(analysisMode);
            }
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
                      </div>
                    </div>
                  </header>
                )}
                quickInsight={detailQuery.data.quickInsight}
                quickStatsLine={quickStatsLine}
                isAnalyzing={isAnalysisPending}
                analysis={analysisData?.analysis}
                persona={analysisData?.persona}
                modeLabel={vibeLabel}
                nextSteps={
                  analysisReady ? (
                    <>
                      <div className={panelStyles.nextStepCard}>
                        <button type="button" className={panelStyles.nextStepButton} onClick={() => void handleShare()}>
                          Share this analysis
                        </button>
                        {shareStatus === "success" && <span className={panelStyles.status}>Link copied.</span>}
                        {shareStatus === "error" && <span className={panelStyles.status}>Couldn't copy link.</span>}
                      </div>

                      <div className={panelStyles.nextStepCard}>
                        <button
                          type="button"
                          className={panelStyles.nextStepButton}
                          onClick={() => setIsVibeChooserOpen((current) => !current)}
                        >
                          Change the vibe
                        </button>
                        {isVibeChooserOpen && (
                          <div className={panelStyles.subAction}>
                            <p>Rerun this player with a different voice or angle.</p>
                            <VibeSelector variant="inline" />
                          </div>
                        )}
                      </div>

                      <div className={panelStyles.nextStepCard}>
                        <button type="button" className={panelStyles.nextStepButton} onClick={handleAnalyzeAnother}>
                          Pick another player
                        </button>
                      </div>
                    </>
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
