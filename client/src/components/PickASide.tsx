import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query";
import { castCompareVote, fetchCompareVotes } from "../api";
import type { CompareVoteState, PlayerType } from "../types";
import { computeVoteShares } from "../utils/votes";
import styles from "../styles/PickASide.module.css";

interface Props {
  playerType: PlayerType;
  players: { id: string; name: string }[];
}

function PickASide({ playerType, players }: Props) {
  const queryClient = useQueryClient();
  const ids = players.map((player) => player.id);
  const queryKey = ["compare-votes", playerType, [...ids].sort().join(",")];

  const votesQuery = useQuery({
    queryKey,
    queryFn: () => fetchCompareVotes(playerType, ids),
    retry: false,
  });

  const voteMutation = useMutation({
    mutationFn: (choice: string) => castCompareVote(playerType, ids, choice),
    onSuccess: (state) => {
      queryClient.setQueryData<CompareVoteState>(queryKey, state);
    },
  });

  // Voting is a bonus. If it is unavailable, stay out of the way.
  if (!votesQuery.data) {
    return null;
  }

  const { yourVote, counts, total } = votesQuery.data;
  const nameById = new Map(players.map((player) => [player.id, player.name]));

  if (!yourVote || !counts) {
    return (
      <div className={styles.container} role="group" aria-label="Who would you take?">
        <span className={styles.heading}>Who would you take?</span>
        <div className={styles.choices}>
          {players.map((player) => (
            <button
              key={player.id}
              type="button"
              className={styles.choice}
              disabled={voteMutation.isPending}
              onClick={() => voteMutation.mutate(player.id)}
            >
              {player.name}
            </button>
          ))}
        </div>
        {voteMutation.isError && <p className={styles.note}>Couldn&apos;t record that vote. Try again in a moment.</p>}
        <p className={styles.note}>See how everyone else voted after you pick. One vote per browser, no sign-in.</p>
      </div>
    );
  }

  const shares = computeVoteShares(ids, counts);

  return (
    <div className={styles.container} role="group" aria-label="How people voted">
      <span className={styles.heading}>How people voted</span>
      <ul className={styles.results}>
        {shares.map((share) => (
          <li key={share.id} className={share.id === yourVote ? styles.rowMine : styles.row}>
            <div className={styles.rowLabel}>
              <span>
                {nameById.get(share.id)}
                {share.id === yourVote && <span className={styles.you}> · your pick</span>}
              </span>
              <span className={styles.percent}>{share.percent}%</span>
            </div>
            <div className={styles.track} aria-hidden="true">
              <div className={styles.fill} style={{ width: `${share.percent}%` }} />
            </div>
          </li>
        ))}
      </ul>
      <p className={styles.note}>
        {total === 1
          ? "You're the first vote on this matchup. Share the link to get more."
          : `${total} votes so far.`}
      </p>
    </div>
  );
}

export default PickASide;
