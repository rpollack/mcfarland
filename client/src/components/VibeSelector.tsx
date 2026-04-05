import { useMemo } from "react";
import { useVibe } from "../contexts/VibeContext";
import styles from "../styles/VibeSelector.module.css";

function VibeSelector() {
  const { vibes, mode, setMode, isLoading } = useVibe();

  const options = useMemo(() => {
    if (!vibes) {
      return [];
    }
    return vibes.map((vibe) => ({
      ...vibe,
      label: formatVibeLabel(vibe.label),
    }));
  }, [vibes]);

  return (
    <div className={styles.container}>
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
    </div>
  );
}

function formatVibeLabel(label: string): string {
  return label.replace(/\s*\(default\)\s*/i, "");
}

export default VibeSelector;
