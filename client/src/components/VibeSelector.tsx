import { useMemo } from "react";
import { useVibe } from "../contexts/vibe";
import { pickSurpriseVibe } from "../utils/vibes";
import styles from "../styles/VibeSelector.module.css";

function VibeSelector() {
  const { vibes, mode, setMode, isLoading } = useVibe();

  const options = useMemo(
    () =>
      vibes.map((vibe) => ({
        id: vibe.id,
        label: formatCompactVibeLabel(vibe.label),
      })),
    [vibes]
  );

  if (options.length === 0) {
    return null;
  }

  const handleSurprise = () => {
    const next = pickSurpriseVibe(
      options.map((option) => option.id),
      mode
    );
    if (next) {
      setMode(next);
    }
  };

  return (
    <div className={styles.container} role="group" aria-label="Hear it as">
      <span className={styles.heading}>Hear it as</span>
      <div className={styles.chips}>
        {options.map((option) => (
          <button
            key={option.id}
            type="button"
            className={option.id === mode ? styles.chipActive : styles.chip}
            aria-pressed={option.id === mode}
            disabled={isLoading}
            onClick={() => setMode(option.id)}
          >
            {option.label}
          </button>
        ))}
        <button type="button" className={styles.surprise} disabled={isLoading} onClick={handleSurprise}>
          <span aria-hidden>🎲</span> Surprise me
        </button>
      </div>
    </div>
  );
}

function formatCompactVibeLabel(label: string): string {
  const normalized = label.replace(/:.*$/, "").replace(/\s*\(default\)\s*/i, "");
  switch (normalized.toLowerCase()) {
    case "straightforward":
      return "Straightforward";
    case "analytics nerd":
      return "Analytics Nerd";
    case "old-timer":
      return "Old-Timer";
    case "gen z":
      return "Gen Z";
    case "old-school":
      return "Old-School";
    case "sensationalist":
      return "Sensationalist";
    case "shakespeare":
      return "Shakespeare";
    case "optimist":
      return "Optimist";
    case "fantasy expert":
      return "Fantasy";
    default:
      return normalized;
  }
}

export default VibeSelector;
