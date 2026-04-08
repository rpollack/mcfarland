import { useEffect, useMemo, useRef, useState } from "react";
import { useVibe } from "../contexts/VibeContext";
import styles from "../styles/VibeSelector.module.css";

function VibeSelector() {
  const { vibes, mode, setMode, isLoading } = useVibe();
  const [isOpen, setIsOpen] = useState(false);
  const containerRef = useRef<HTMLDivElement | null>(null);

  const options = useMemo(() => {
    if (!vibes) {
      return [];
    }

    return vibes.map((vibe) => ({
      id: vibe.id,
      label: formatCompactVibeLabel(vibe.label),
    }));
  }, [vibes]);

  const activeLabel = options.find((option) => option.id === mode)?.label ?? "Choose a vibe";

  useEffect(() => {
    if (!isOpen) {
      return;
    }

    const handlePointerDown = (event: MouseEvent) => {
      if (!containerRef.current?.contains(event.target as Node)) {
        setIsOpen(false);
      }
    };

    const handleEscape = (event: KeyboardEvent) => {
      if (event.key === "Escape") {
        setIsOpen(false);
      }
    };

    document.addEventListener("mousedown", handlePointerDown);
    document.addEventListener("keydown", handleEscape);
    return () => {
      document.removeEventListener("mousedown", handlePointerDown);
      document.removeEventListener("keydown", handleEscape);
    };
  }, [isOpen]);

  return (
    <div className={styles.container} ref={containerRef}>
      <button
        type="button"
        className={styles.trigger}
        onClick={() => setIsOpen((current) => !current)}
        disabled={isLoading || options.length === 0}
        aria-haspopup="listbox"
        aria-expanded={isOpen}
      >
        <span aria-hidden className={styles.icon}>🎭</span>
        <span>Change the vibe</span>
      </button>

      {isOpen && (
        <div className={styles.menu} role="listbox" aria-label="Analysis vibes">
          <div className={styles.menuHeader}>
            <span className={styles.menuTitle}>Current: {activeLabel}</span>
            <button type="button" className={styles.closeButton} onClick={() => setIsOpen(false)}>
              Close
            </button>
          </div>

          <div className={styles.optionList}>
            {options.map((option) => (
              <button
                key={option.id}
                type="button"
                className={option.id === mode ? styles.optionActive : styles.option}
                onClick={() => {
                  setMode(option.id);
                  setIsOpen(false);
                }}
              >
                <span>{option.label}</span>
                {option.id === mode && <span className={styles.check}>✓</span>}
              </button>
            ))}
          </div>
        </div>
      )}
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
