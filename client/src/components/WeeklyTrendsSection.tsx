import { useQuery } from "@tanstack/react-query";
import { fetchWeeklyTrends } from "../api";
import type { TrendPlayer, PlayerType } from "../types";
import PlayerHeadshot from "./PlayerHeadshot";
import styles from "../styles/WeeklyTrendsSection.module.css";

type Props = {
  onSelectPlayer: (player: { playerType: PlayerType; playerId: string }) => void;
};

function TrendGroup({
  title,
  players,
  onSelectPlayer,
}: {
  title: string;
  players: TrendPlayer[];
  onSelectPlayer: (player: { playerType: PlayerType; playerId: string }) => void;
}) {
  return (
    <section className={styles.group} aria-label={title}>
      <h4 className={styles.groupTitle}>{title}</h4>
      <div className={styles.buttonList}>
        {players.map((player) => (
          <button
            key={`${title}-${player.id}`}
            type="button"
            className={styles.playerButton}
            onClick={() => onSelectPlayer({ playerType: player.type, playerId: player.id })}
          >
            <PlayerHeadshot
              name={player.name}
              playerId={player.id}
              mlbamid={player.mlbamid}
              size={24}
            />
            <span>{player.name}</span>
          </button>
        ))}
      </div>
    </section>
  );
}

export default function WeeklyTrendsSection({ onSelectPlayer }: Props) {
  const trendsQuery = useQuery({
    queryKey: ["weekly-trends"],
    queryFn: fetchWeeklyTrends,
    staleTime: 30 * 60_000,
  });

  if (trendsQuery.isLoading) {
    return (
      <section className={styles.wrapper} aria-label="Weekly risers and fallers">
        <h3 className={styles.title}>Monday Analysis Starter Pack</h3>
        <p className={styles.subhead}>Loading this week&apos;s risers and fallers…</p>
      </section>
    );
  }

  if (trendsQuery.isError || !trendsQuery.data) {
    return null;
  }

  return (
    <section className={styles.wrapper} aria-label="Weekly risers and fallers">
      <h3 className={styles.title}>Monday Analysis Starter Pack</h3>
      <p className={styles.subhead}>
        Quick picks from last week vs. the week before. Click any player to run a full analysis.
      </p>

      <div className={styles.columns}>
        <div className={styles.column}>
          <h3 className={styles.columnTitle}>Hitters</h3>
          <TrendGroup
            title="Risers"
            players={trendsQuery.data.hitters.risers}
            onSelectPlayer={onSelectPlayer}
          />
          <TrendGroup
            title="Fallers"
            players={trendsQuery.data.hitters.fallers}
            onSelectPlayer={onSelectPlayer}
          />
        </div>

        <div className={styles.column}>
          <h3 className={styles.columnTitle}>Pitchers</h3>
          <TrendGroup
            title="Risers"
            players={trendsQuery.data.pitchers.risers}
            onSelectPlayer={onSelectPlayer}
          />
          <TrendGroup
            title="Fallers"
            players={trendsQuery.data.pitchers.fallers}
            onSelectPlayer={onSelectPlayer}
          />
        </div>
      </div>
    </section>
  );
}

