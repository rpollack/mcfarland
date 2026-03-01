import { useEffect, useState } from "react";
import { useQuery } from "@tanstack/react-query";
import clsx from "clsx";
import { fetchWeeklyTrends } from "../api";
import type { TrendPlayer, PlayerType } from "../types";
import PlayerHeadshot from "./PlayerHeadshot";
import styles from "../styles/WeeklyTrendsSection.module.css";

type Props = {
  playerType: PlayerType;
  onSelectPlayer: (playerId: string) => void;
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
  onSelectPlayer: (playerId: string) => void;
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
            onClick={() => onSelectPlayer(player.id)}
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

export default function WeeklyTrendsSection({ playerType, onSelectPlayer, embedded = false }: Props) {
  const [collapsedOnMobile, setCollapsedOnMobile] = useState(false);

  useEffect(() => {
    if (typeof window === "undefined") {
      return;
    }
    setCollapsedOnMobile(window.innerWidth < 640);
  }, []);

  const trendsQuery = useQuery({
    queryKey: ["weekly-trends"],
    queryFn: fetchWeeklyTrends,
    staleTime: 30 * 60_000,
  });

  if (trendsQuery.isLoading) {
    return (
      <section className={clsx(styles.wrapper, embedded && styles.embedded)} aria-label="Weekly risers and fallers">
        <p className={styles.subhead}>Loading…</p>
      </section>
    );
  }

  if (trendsQuery.isError || !trendsQuery.data) {
    return null;
  }

  const trendBucket = playerType === "hitter" ? trendsQuery.data.hitters : trendsQuery.data.pitchers;
  const onFirePlayers = trendBucket.risers;
  const iceColdPlayers = trendBucket.fallers;

  return (
    <section className={clsx(styles.wrapper, embedded && styles.embedded)} aria-label="Weekly risers and fallers">
      <button
        type="button"
        className={styles.mobileToggle}
        aria-expanded={!collapsedOnMobile}
        onClick={() => setCollapsedOnMobile((value) => !value)}
      >
        {collapsedOnMobile ? "Show quick links" : "Hide quick links"}
      </button>

      <div className={clsx(styles.rows, collapsedOnMobile && styles.rowsCollapsed)}>
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
      </div>
    </section>
  );
}
