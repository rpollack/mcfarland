import { useEffect, useState } from "react";
import type { PlayerType } from "../types";
import {
  getStarredPlayersUpdatedEventName,
  isPlayerStarred,
  toggleStarredPlayer,
} from "../utils/starredPlayers";
import styles from "../styles/StarButton.module.css";

interface Props {
  playerId: string;
  playerName: string;
  playerType: PlayerType;
  mlbamid?: string | null;
}

function StarButton({ playerId, playerName, playerType, mlbamid }: Props) {
  const [starred, setStarred] = useState(() => isPlayerStarred(playerId, playerType));
  const [syncedKey, setSyncedKey] = useState(`${playerType}|${playerId}`);

  // Re-read when the shown player changes, without an extra effect-driven render.
  if (syncedKey !== `${playerType}|${playerId}`) {
    setSyncedKey(`${playerType}|${playerId}`);
    setStarred(isPlayerStarred(playerId, playerType));
  }

  useEffect(() => {
    const eventName = getStarredPlayersUpdatedEventName();
    const refresh = () => setStarred(isPlayerStarred(playerId, playerType));
    window.addEventListener(eventName, refresh);
    return () => window.removeEventListener(eventName, refresh);
  }, [playerId, playerType]);

  return (
    <button
      type="button"
      className={starred ? styles.starActive : styles.star}
      aria-pressed={starred}
      aria-label={starred ? `Remove ${playerName} from your starred players` : `Star ${playerName}`}
      title={starred ? "Starred on this device. Tap to remove." : "Star this player. Saved on this device only."}
      onClick={() => toggleStarredPlayer({ playerId, playerName, playerType, mlbamid: mlbamid ?? null })}
    >
      <svg width="20" height="20" viewBox="0 0 24 24" aria-hidden="true" focusable="false">
        <path
          d="M12 3l2.7 5.6 6.1.9-4.4 4.3 1 6.1L12 17l-5.4 2.9 1-6.1L3.2 9.5l6.1-.9z"
          fill={starred ? "currentColor" : "none"}
          stroke="currentColor"
          strokeWidth="2"
          strokeLinejoin="round"
        />
      </svg>
    </button>
  );
}

export default StarButton;
