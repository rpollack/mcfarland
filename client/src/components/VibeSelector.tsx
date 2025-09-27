import { useMemo } from "react";
import { useVibe } from "../contexts/VibeContext";
import styles from "../styles/VibeSelector.module.css";

function VibeSelector() {
  const { vibes, mode, setMode, isLoading } = useVibe();

  const options = useMemo(() => vibes ?? [], [vibes]);

  return (
    <div className={styles.container}>
      <label htmlFor="vibe-select">Vibe</label>
      <select
        id="vibe-select"
        value={mode}
        onChange={(event) => setMode(event.target.value)}
        disabled={isLoading || options.length === 0}
      >
        {options.map((vibe) => (
          <option key={vibe.id} value={vibe.id}>
            {vibe.label}
          </option>
        ))}
      </select>
      <p className={styles.helper}>
        {isLoading ? "Loading vibes..." : options.find((vibe) => vibe.id === mode)?.description ?? ""}
      </p>
    </div>
  );
}

export default VibeSelector;
