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
  embedded?: boolean;
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
  embedded = false,
}: Props) {
  const handlePlayerSelect = (player: PlayerSummary) => {
    onSelect(player.id);
    onSearchTermChange("");
  };

  return (
    <section className={embedded ? styles.panelEmbedded : styles.panel} aria-label="Player search">
      <h3 className={styles.heading}>Select a player</h3>
      <p className={styles.subheading}>Type a player name to run analysis.</p>
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
      <input
        id="player-search"
        type="search"
        value={searchTerm}
        onChange={(event) => onSearchTermChange(event.target.value)}
        placeholder={`Find ${playerTypeLabels[playerType].toLowerCase()}...`}
        className={styles.search}
        autoFocus
      />
      <ul className={styles.results} aria-live="polite">
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
