import { useEffect, useMemo, useRef, useState } from "react";
import { useVibe } from "../contexts/VibeContext";
import styles from "../styles/VibeSelector.module.css";

interface Props {
  variant?: "popover" | "inline";
}

function VibeSelector({ variant = "popover" }: Props) {
  const { vibes, mode, setMode, isLoading } = useVibe();
  const [isOpen, setIsOpen] = useState(false);
  const containerRef = useRef<HTMLDivElement | null>(null);

  const options = useMemo(() =>{
    if (!vibes) {
      return [];
    }
    return vibes.map((vibe) => ({
      ...vibe,
      label: formatVibeLabel(vibe.label),
      compactLabel: formatCompactVibeLabel(vibe.label),
    }));
  }, [vibes]);

  const selectedOption = options.find((option) => option.id === mode) ?? options[0];
  const optionListClassName = variant === "inline" ? styles.inlineList : styles.popover;

  useEffect(() => {
    if (variant !== "popover" || !isOpen) {
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

  if (variant === "inline") {
    return (
      <div className={styles.container}>
        <div className={styles.inlineList} role="listbox" aria-label="Analysis vibes">
          {options.map((vibe) => (
            <button
              key={vibe.id}
              type="button"
              className={vibe.id === mode ? styles.optionActive : styles.option}
              onClick={() => {
                setMode(vibe.id);
              }}
              disabled={isLoading || options.length === 0}
            >
              <span className={styles.optionLabel}>{vibe.compactLabel}</span>
            </button>
          ))}
        </div>
      </div>
    );
  }

  return (
    <div className={styles.container} ref={containerRef}>
      <button
        type="button"
        className={styles.trigger}
        onClick={() => setIsOpen((current) => !current)}
        disabled={isLoading || options.length === 0}
        aria-haspopup="listbox"
        aria-expanded={isOpen}
        aria-label="Choose analysis vibe"
      >
        <span className={styles.triggerLabel}>{selectedOption?.label ?? "Choose a vibe"}</span>
        <span className={styles.chevron} aria-hidden>⌄</span>
      </button>

      {isOpen && (
        <div className={optionListClassName} role="listbox" aria-label="Analysis vibes">
          {options.map((vibe) => (
            <button
              key={vibe.id}
              type="button"
              className={vibe.id === mode ? styles.optionActive : styles.option}
              onClick={() => {
                setMode(vibe.id);
                setIsOpen(false);
              }}
            >
              <span className={styles.optionLabel}>{vibe.label}</span>
              <span className={styles.optionDescription}>{vibe.description}</span>
            </button>
          ))}
        </div>
      )}
    </div>
  );
}

function formatVibeLabel(label: string): string {
  const normalized = label.replace(/:.*$/, "").replace(/\s*\(default\)\s*/i, "");
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

function formatCompactVibeLabel(label: string): string {
  const normalized = label.replace(/:.*$/, "").replace(/\s*\(default\)\s*/i, "");
  switch (normalized.toLowerCase()) {
    case "straightforward":
      return "➡️ Straightforward";
    case "analytics dork":
      return "📊 Analytics dork";
    case "old coot":
      return "🧓 Old coot";
    case "gen z":
      return "🌀 Gen Z";
    case "seventies":
      return "🎷 Seventies";
    case "sensationalist":
      return "🎪 Sensationalist";
    case "shakespeare":
      return "🎭 Shakespeare";
    case "rose-colored glasses":
      return "🌹 Rose-colored";
    case "rotisserie expert":
      return "🍗 Rotisserie";
    default:
      return `✨ ${normalized}`;
  }
}

export default VibeSelector;
