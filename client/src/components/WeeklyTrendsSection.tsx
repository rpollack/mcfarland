import { useEffect, useState } from "react";
import { useQuery } from "@tanstack/react-query";
import clsx from "clsx";
import { fetchTrendingQuickLinks, fetchWeeklyTrends } from "../api";
import type { TrendPlayer, PlayerType } from "../types";
import {
  getRecentAnalysesUpdatedEventName,
  readRecentAnalyses,
  type RecentAnalysisEntry,
} from "../utils/recentAnalyses";
import PlayerHeadshot from "./PlayerHeadshot";
import styles from "../styles/WeeklyTrendsSection.module.css";

type Props = {
  playerType: PlayerType;
  onSelectPlayer: (selection: {
    playerId: string;
    source: "recent_analyze_again" | "main_ui";
    analysisMode?: string;
  }) => void;
  embedded?: boolean;
};

function TrendGroup({
  title,
  emoji,
  players,
  onSelectPlayer,
}: {
  title: string;
  emoji: string;
  players: TrendPlayer[];
  onSelectPlayer: Props["onSelectPlayer"];
}) {
  return (
    <section className={styles.row} aria-label={title}>
      <p className={styles.rowLeadIn}>
        <span aria-hidden="true">{emoji}</span> {title}:
      </p>
      <div className={styles.chipList}>
        {players.map((player) => (
          <button
            key={`${title}-${player.id}`}
            type="button"
            className={styles.chipButton}
            onClick={() => onSelectPlayer({
              playerId: player.id,
              source: "main_ui",
            })}
          >
            <PlayerHeadshot
              name={player.name}
              playerId={player.id}
              mlbamid={player.mlbamid}
              size={20}
            />
            <span>{player.name}</span>
          </button>
        ))}
      </div>
    </section>
  );
}

function RecentAnalysesGroup({
  analyses,
  onSelectPlayer,
}: {
  analyses: RecentAnalysisEntry[];
  onSelectPlayer: Props["onSelectPlayer"];
}) {
  if (analyses.length === 0) {
    return null;
  }

  return (
    <section className={styles.row} aria-label="Analyze again">
      <p className={styles.rowLeadIn}>
        <span aria-hidden="true">🔄</span> Analyze Again:
      </p>
      <div className={styles.chipList}>
        {analyses.map((analysis) => (
          <button
            key={`${analysis.playerId}-${analysis.analysisMode}`}
            type="button"
            className={styles.chipButton}
            onClick={() => onSelectPlayer({
              playerId: analysis.playerId,
              analysisMode: analysis.analysisMode,
              source: "recent_analyze_again",
            })}
            title={`${analysis.playerName} (${analysis.analysisMode})`}
          >
            <PlayerHeadshot
              name={analysis.playerName}
              playerId={analysis.playerId}
              mlbamid={analysis.mlbamid}
              size={20}
            />
            <span>{analysis.playerName}</span>
          </button>
        ))}
      </div>
    </section>
  );
}

export default function WeeklyTrendsSection({ playerType, onSelectPlayer, embedded = false }: Props) {
  const [collapsedOnMobile, setCollapsedOnMobile] = useState(false);
  const [recentAnalyses, setRecentAnalyses] = useState<RecentAnalysisEntry[]>([]);

  useEffect(() => {
    if (typeof window === "undefined") {
      return;
    }
    setCollapsedOnMobile(window.innerWidth < 640);
  }, []);

  useEffect(() => {
    if (typeof window === "undefined") {
      return;
    }
    const eventName = getRecentAnalysesUpdatedEventName();
    const syncRecentAnalyses = () => {
      setRecentAnalyses(
        readRecentAnalyses().filter((analysis) => analysis.playerType === playerType)
      );
    };

    syncRecentAnalyses();
    window.addEventListener(eventName, syncRecentAnalyses);
    return () => {
      window.removeEventListener(eventName, syncRecentAnalyses);
    };
  }, [playerType]);

  const trendsQuery = useQuery({
    queryKey: ["weekly-trends"],
    queryFn: fetchWeeklyTrends,
    staleTime: 30 * 60_000,
  });

  const trendingQuery = useQuery({
    queryKey: ["trending-quick-links"],
    queryFn: fetchTrendingQuickLinks,
    staleTime: 30 * 60_000,
  });

  const trendBucket =
    trendsQuery.data &&
    (playerType === "hitter" ? trendsQuery.data.hitters : trendsQuery.data.pitchers);
  const trendingBucket =
    trendingQuery.data &&
    (playerType === "hitter" ? trendingQuery.data.hitters : trendingQuery.data.pitchers);
  const trendingPlayers = trendingBucket?.trending ?? [];
  const breakoutPlayers = trendingBucket?.breakouts ?? [];
  const onFirePlayers = trendBucket?.risers ?? [];
  const iceColdPlayers = trendBucket?.fallers ?? [];

  return (
    <section className={clsx(styles.wrapper, embedded && styles.embedded)} aria-label="Quick links">
      <button
        type="button"
        className={styles.mobileToggle}
        aria-expanded={!collapsedOnMobile}
        onClick={() => setCollapsedOnMobile((value) => !value)}
      >
        {collapsedOnMobile ? "Show quick links" : "Hide quick links"}
      </button>

      <div className={clsx(styles.rows, collapsedOnMobile && styles.rowsCollapsed)}>
        <RecentAnalysesGroup analyses={recentAnalyses} onSelectPlayer={onSelectPlayer} />
        {(trendsQuery.isLoading || trendingQuery.isLoading) && <p className={styles.subhead}>Loading…</p>}
        {(trendsQuery.isError || trendingQuery.isError) && <p className={styles.subhead}>Unable to load quick links right now.</p>}
        {!trendingQuery.isLoading && !trendingQuery.isError && breakoutPlayers.length > 0 && (
          <TrendGroup
            title="Breakouts"
            emoji="🚀"
            players={breakoutPlayers}
            onSelectPlayer={onSelectPlayer}
          />
        )}
        {!trendingQuery.isLoading && !trendingQuery.isError && trendingPlayers.length > 0 && (
          <TrendGroup
            title="Out of Character"
            emoji="👀"
            players={trendingPlayers}
            onSelectPlayer={onSelectPlayer}
          />
        )}
        {!trendsQuery.isLoading && !trendsQuery.isError && (
          <>
            <TrendGroup
              title="On Fire"
              emoji="🔥"
              players={onFirePlayers}
              onSelectPlayer={onSelectPlayer}
            />
            <TrendGroup
              title="Ice Cold"
              emoji="🥶"
              players={iceColdPlayers}
              onSelectPlayer={onSelectPlayer}
            />
          </>
        )}
      </div>
    </section>
  );
}
