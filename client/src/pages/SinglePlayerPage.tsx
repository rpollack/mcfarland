import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { useMutation, useQuery } from "@tanstack/react-query";
import PlayerPicker from "../components/PlayerPicker";
import AnalysisPanel from "../components/AnalysisPanel";
import type { QuickStatSignal } from "../components/AnalysisPanel";
import VibeSelector from "../components/VibeSelector";
import PlayerHeadshot from "../components/PlayerHeadshot";
import WeeklyTrendsSection from "../components/WeeklyTrendsSection";
import panelStyles from "../styles/AnalysisPanel.module.css";
import { useVibe } from "../contexts/VibeContext";
import {
  analyzePlayer,
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

const SEARCH_MIN_LENGTH = 2;
const SEARCH_RESULT_LIMIT = 8;

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

function formatAge(value: number | null | undefined): string | undefined {
  if (value === null || value === undefined || Number.isNaN(value)) {
    return undefined;
  }
  return String(Math.round(value));
}

function isFiniteNumber(value: number | null | undefined): value is number {
  return value !== null && value !== undefined && Number.isFinite(value);
}

function formatSignedValue(value: number, digits: number): string {
  const prefix = value > 0 ? "+" : "";
  return `${prefix}${Number(value).toFixed(digits)}`.replace(/^([+-]?)0\./, "$1.");
}

type SignalCandidate = QuickStatSignal & {
  score: number;
};

function buildSignal(
  label: string,
  value: number | null | undefined,
  digits: number,
  scoreMultiplier: number,
  goodDirection: 1 | -1 | 0 = 1
): SignalCandidate | null {
  if (!isFiniteNumber(value) || Math.abs(value) < 0.0005) {
    return null;
  }

  const tone =
    goodDirection === 0
      ? "neutral"
      : value * goodDirection > 0
        ? "positive"
        : "negative";

  return {
    label: `${label} ${formatSignedValue(value, digits)}`,
    score: Math.abs(value) * scoreMultiplier,
    tone,
  };
}

function formatCurrentValue(value: number, digits: number, suffix = ""): string {
  return `${Number(value).toFixed(digits)}${suffix}`.replace(/^(-?)0\./, "$1.");
}

function buildCurrentSignal(label: string, value: number | null | undefined, digits: number, suffix = ""): QuickStatSignal | null {
  if (!isFiniteNumber(value)) {
    return null;
  }

  return {
    label: `${label} ${formatCurrentValue(value, digits, suffix)}`,
    tone: "neutral",
  };
}

function buildQuickStatSignals(player: HitterRecord | PitcherRecord, playerType: PlayerType): QuickStatSignal[] {
  const signals =
    playerType === "hitter"
      ? [
          buildSignal("xwOBA", (player as HitterRecord).xwOBA_diff, 3, 100),
          buildSignal("wOBA", (player as HitterRecord).wOBA_diff, 3, 100),
          buildSignal("Barrel%", (player as HitterRecord).Barrel_pct_diff, 1, 1),
          buildSignal("K%", (player as HitterRecord).K_pct_diff, 1, 1, -1),
          buildSignal("BB%", (player as HitterRecord).BB_pct_diff, 1, 1),
          buildSignal("BABIP", (player as HitterRecord).BABIP_diff, 3, 100, 0),
          buildSignal("xwOBA-wOBA", (player as HitterRecord).xwOBA_wOBA_gap_diff, 3, 100),
        ]
      : [
          buildSignal("xERA", (player as PitcherRecord).xera_diff, 2, 1, -1),
          buildSignal("ERA", (player as PitcherRecord).era_diff, 2, 1, -1),
          buildSignal("K-BB%", (player as PitcherRecord).k_minus_bb_percent_diff, 1, 1),
          buildSignal("CSW%", (player as PitcherRecord).csw_percent_diff, 1, 1),
          buildSignal("Barrel%", (player as PitcherRecord).barrel_percent_diff, 1, 1, -1),
          buildSignal("BABIP", (player as PitcherRecord).babip_diff, 3, 100, 0),
          buildSignal("LOB%", (player as PitcherRecord).lob_percent_diff, 1, 1, 0),
        ];

  const diffSignals = signals
    .filter((signal): signal is SignalCandidate => Boolean(signal))
    .sort((a, b) => b.score - a.score)
    .slice(0, 3)
    .map(({ label, tone }) => ({ label, tone }));

  if (diffSignals.length > 0) {
    return diffSignals;
  }

  const fallbackSignals =
    playerType === "hitter"
      ? [
          buildCurrentSignal("xwOBA", (player as HitterRecord).xwOBA_cur, 3),
          buildCurrentSignal("wOBA", (player as HitterRecord).wOBA_cur, 3),
          buildCurrentSignal("Barrel%", (player as HitterRecord).Barrel_pct_cur, 1, "%"),
        ]
      : [
          buildCurrentSignal("xERA", (player as PitcherRecord).xera_cur, 2),
          buildCurrentSignal("K-BB%", (player as PitcherRecord).k_minus_bb_percent_cur, 1, "%"),
          buildCurrentSignal("CSW%", (player as PitcherRecord).csw_percent_cur, 1, "%"),
        ];

  return fallbackSignals.filter((signal): signal is QuickStatSignal => Boolean(signal));
}

function buildPlayerMetaLine(
  player: HitterRecord | PitcherRecord,
  playerType: PlayerType,
  ageLabel?: string
): string | undefined {
  const prefix = ageLabel ? `Age ${ageLabel} · ` : "";

  if (playerType === "hitter") {
    const hitter = player as HitterRecord;
    return `${prefix}${formatSlashStat(hitter.AVG_cur)}/${formatSlashStat(hitter.OBP_cur)}/${formatSlashStat(hitter.SLG_cur)} in ${formatInteger(hitter.PA_cur)} PA`;
  }

  const pitcher = player as PitcherRecord;
  return `${prefix}${formatEra(pitcher.era_cur)} ERA, ${formatInteger(pitcher.so)} SO, ${formatInteger(pitcher.bb)} BB in ${formatInteger(pitcher.tbf)} TBF`;
}

function SinglePlayerExperience({ initialPlayerType, initialPlayerId, onStateChange }: Props) {
  const { mode, setMode, vibes } = useVibe();
  const [playerType, setPlayerType] = useState<PlayerType>(initialPlayerType);
  const [searchTerm, setSearchTerm] = useState("");
  const [selectedId, setSelectedId] = useState<string | undefined>(initialPlayerId);
  const [selectionSource, setSelectionSource] = useState<"recent_analyze_again" | "main_ui">("main_ui");
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

  const trimmedSearch = searchTerm.trim();
  const searchEnabled = trimmedSearch.length >= SEARCH_MIN_LENGTH;

  const playersQuery = useQuery({
    queryKey: ["players", playerType, trimmedSearch],
    queryFn: () => fetchPlayers(playerType, trimmedSearch, SEARCH_RESULT_LIMIT),
    enabled: searchEnabled,
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
  const quickStatSignals = detailQuery.data
    ? buildQuickStatSignals(detailQuery.data.player, playerType)
    : [];
  const selectedPlayerAge = detailQuery.data ? formatAge(detailQuery.data.player.Age) : undefined;
  const selectedPlayerMetaLine = detailQuery.data
    ? buildPlayerMetaLine(detailQuery.data.player, playerType, selectedPlayerAge)
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
          players={searchEnabled ? playersQuery.data ?? [] : []}
          selectedId={selectedId}
          onSelect={(playerId) => {
            setSelectionSource("main_ui");
            setSelectedId(playerId);
          }}
          isLoading={searchEnabled && playersQuery.isLoading}
        />
        <WeeklyTrendsSection
          embedded
          playerType={playerType}
          onSelectPlayer={({ playerId, playerType: selectedPlayerType, source, analysisMode }) => {
            setSelectionSource(source);
            if (selectedPlayerType && selectedPlayerType !== playerType) {
              setPlayerType(selectedPlayerType);
            }
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
                        size={56}
                        className={styles.analysisHeadshot}
                      />
                      <div className={styles.playerMeta}>
                        <h2>{detailQuery.data.player.Name}</h2>
                        {selectedPlayerMetaLine && <p>{selectedPlayerMetaLine}</p>}
                      </div>
                    </div>
                  </header>
                )}
                quickInsight={detailQuery.data.quickInsight}
                quickStatSignals={quickStatSignals}
                isAnalyzing={isAnalysisPending}
                headline={analysisData?.headline}
                analysis={analysisData?.analysis}
                persona={analysisData?.persona}
                modeLabel={vibeLabel}
                nextSteps={
                  analysisReady ? (
                    <>
                      <div className={panelStyles.nextStepCard}>
                        <button type="button" className={panelStyles.nextStepButton} onClick={() => void handleShare()}>
                          <span aria-hidden className={panelStyles.icon}>🔗</span>
                          <span>Share this analysis</span>
                        </button>
                        {shareStatus === "success" && <span className={panelStyles.status}>Link copied.</span>}
                        {shareStatus === "error" && <span className={panelStyles.status}>Couldn't copy link.</span>}
                      </div>

                      <div className={panelStyles.nextStepCard}>
                        <VibeSelector />
                      </div>

                      <div className={panelStyles.nextStepCard}>
                        <button type="button" className={panelStyles.nextStepButton} onClick={handleAnalyzeAnother}>
                          <span aria-hidden className={panelStyles.icon}>🔎</span>
                          <span>Pick another player</span>
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
