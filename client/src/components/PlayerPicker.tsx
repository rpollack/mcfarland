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
  const handlePlayerSelect = (player: PlayerSummary) => {
    onSelect(player.id);
    onSearchTermChange("");
  };

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
      <ul className={styles.results} aria-live="polite">
        {searchTerm.trim().length === 0 && !isLoading && !selectedId && (
          <li className={styles.helper}>Start typing to pick a player.</li>
        )}
        {isLoading && <li className={styles.helper}>Loading players…</li>}
        {!isLoading && searchTerm && players.length === 0 && (
          <li className={styles.helper}>No players found.</li>
        )}
        {!isLoading &&
          searchTerm.trim() &&
          players.slice(0, 8).map((player) => (
            <li key={player.id}>
              <button
                type="button"
                onClick={() => handlePlayerSelect(player)}
                className={
                  player.id === selectedId ? styles.selectedResult : styles.resultButton
                }
              >
                {player.name}
              </button>
            </li>
          ))}
        {selectedId && !searchTerm && !isLoading && (
          <li className={styles.helper}>Search again to switch players.</li>
        )}
      </ul>
    </section>
  );
}

export default PlayerPicker;
