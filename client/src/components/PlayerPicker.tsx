import type { PlayerSummary, PlayerType } from "../types";
import styles from "../styles/PlayerPicker.module.css";

interface Props {
  playerType: PlayerType;
  onTypeChange: (type: PlayerType) => void;
  searchTerm: string;
  onSearchTermChange: (value: string) => void;
  players: PlayerSummary[];
  selectedId?: string;
  onSelect: (playerId: string | undefined) => void;
  isLoading: boolean;
}

const playerTypeLabels: Record<PlayerType, string> = {
  hitter: "Hitters",
  pitcher: "Pitchers",
};

function PlayerPicker({
  playerType,
  onTypeChange,
  searchTerm,
  onSearchTermChange,
  players,
  selectedId,
  onSelect,
  isLoading,
}: Props) {
  return (
    <section className={styles.panel} aria-label="Player search">
      <div className={styles.typeToggle} role="group" aria-label="Player type selector">
        {(Object.keys(playerTypeLabels) as PlayerType[]).map((type) => (
          <button
            key={type}
            type="button"
            onClick={() => onTypeChange(type)}
            className={type === playerType ? styles.activeType : styles.typeButton}
          >
            {playerTypeLabels[type]}
          </button>
        ))}
      </div>
      <label htmlFor="player-search" className={styles.label}>
        Search by player name
      </label>
      <input
        id="player-search"
        type="search"
        value={searchTerm}
        onChange={(event) => onSearchTermChange(event.target.value)}
        placeholder={`Find ${playerTypeLabels[playerType].toLowerCase()}`}
        className={styles.search}
      />
      <label htmlFor="player-select" className={styles.label}>
        Select player
      </label>
      <select
        id="player-select"
        value={selectedId ?? ""}
        onChange={(event) => onSelect(event.target.value || undefined)}
        className={styles.select}
        size={Math.min(8, Math.max(players.length || 3, 3))}
      >
        <option value="">Select a player</option>
        {isLoading ? (
          <option disabled>Loading players...</option>
        ) : players.length === 0 ? (
          <option disabled>No players found</option>
        ) : (
          players.map((player) => (
            <option key={player.id} value={player.id}>
              {player.name}
            </option>
          ))
        )}
      </select>
    </section>
  );
}

export default PlayerPicker;
