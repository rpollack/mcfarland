import { useQuery } from "@tanstack/react-query";
import { Link } from "react-router-dom";
import { fetchRelatedPlayers, logUiEvent } from "../api";
import type { PlayerType } from "../types";
import { buildCompareHref, formatLuckGap } from "../utils/keepGoing";
import styles from "../styles/KeepGoing.module.css";

interface Props {
  playerId: string;
  playerType: PlayerType;
  onSelectPlayer: (playerId: string) => void;
}

const GAP_HINT: Record<PlayerType, string> = {
  hitter: "Luck gap is expected wOBA minus actual wOBA. Positive means the results trail the contact quality.",
  pitcher: "Luck gap is ERA minus expected ERA. Positive means the results are worse than the underlying skills.",
};

function KeepGoing({ playerId, playerType, onSelectPlayer }: Props) {
  const relatedQuery = useQuery({
    queryKey: ["related-players", playerType, playerId],
    queryFn: () => fetchRelatedPlayers(playerType, playerId),
    staleTime: 5 * 60 * 1000,
  });

  const related = relatedQuery.data;
  if (!related || (related.similarGap.length === 0 && !related.rival)) {
    return null;
  }

  const { rival } = related;

  return (
    <div className={styles.container} role="group" aria-label="Keep going">
      <span className={styles.heading}>Keep going</span>
      <div className={styles.chips}>
        {related.similarGap.map((player, index) => (
          <button
            key={player.id}
            type="button"
            className={styles.chip}
            onClick={() => {
              void logUiEvent({
                event: "keep_going_clicked",
                playerType,
                kind: "similar_gap",
                rank: index + 1,
                sourcePlayerId: playerId,
                targetPlayerId: player.id,
                targetPlayerName: player.name,
              });
              onSelectPlayer(player.id);
            }}
          >
            Similar luck gap: {player.name}
            {player.gap !== null && (
              <>
                {" "}
                <span className={styles.gap}>({formatLuckGap(player.gap, playerType)})</span>
              </>
            )}
          </button>
        ))}
        {rival && (
          <Link
            className={styles.compare}
            to={buildCompareHref(playerType, [playerId, rival.id])}
            onClick={() =>
              void logUiEvent({
                event: "keep_going_clicked",
                playerType,
                kind: "rival_compare",
                rank: 1,
                sourcePlayerId: playerId,
                targetPlayerId: rival.id,
                targetPlayerName: rival.name,
              })
            }
          >
            Compare with {rival.name}
          </Link>
        )}
      </div>
      {related.similarGap.length > 0 && <p className={styles.hint}>{GAP_HINT[playerType]}</p>}
    </div>
  );
}

export default KeepGoing;
