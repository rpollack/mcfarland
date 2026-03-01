import { useMemo } from "react";
import { useVibe } from "../contexts/VibeContext";
import styles from "../styles/VibeSelector.module.css";

function VibeSelector() {
  const { vibes, mode, setMode, isLoading } = useVibe();

  const options = useMemo(() =>{
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
  const normalized = label.replace(/:.*$/, "");
  switch (normalized.toLowerCase()) {
    case "straightforward":
      return "➡️ Straightforward — Just the facts";
    case "analytics dork":
      return "📊 Analytics dork — Numbers first";
    case "old coot":
      return "🧓 Old coot — Get off my lawn";
    case "gen z":
      return "🌀 Gen Z — Meme energy";
    case "seventies":
      return "🎷 Seventies — Retro vibes";
    case "sensationalist":
      return "🎪 Sensationalist — Big drama";
    case "shakespeare":
      return "🎭 Shakespeare — Bard mode";
    case "rose-colored glasses":
      return "🌹 Rose-colored — Always sunny";
    case "rotisserie expert":
      return "🍗 Rotisserie — Fantasy focus";
    default:
      return `✨ ${label}`;
  }
}

export default VibeSelector;
