import { useEffect, useMemo, useRef, useState } from "react";
import { useVibe } from "../contexts/VibeContext";
import styles from "../styles/VibeSelector.module.css";

interface Props {
  variant?: "popover" | "inline";
  open?: boolean;
  onOpenChange?: (open: boolean) => void;
}

function VibeSelector({ variant = "popover", open, onOpenChange }: Props) {
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
  const controlledOpen = open ?? isOpen;

  const setOpenState = (nextOpen: boolean) => {
    if (open === undefined) {
      setIsOpen(nextOpen);
    }
    onOpenChange?.(nextOpen);
  };

  useEffect(() => {
    if (variant !== "popover" || !controlledOpen) {
      return;
    }

    const handlePointerDown = (event: MouseEvent) => {
      if (!containerRef.current?.contains(event.target as Node)) {
        setOpenState(false);
      }
    };

    const handleEscape = (event: KeyboardEvent) => {
      if (event.key === "Escape") {
        setOpenState(false);
      }
    };

    document.addEventListener("mousedown", handlePointerDown);
    document.addEventListener("keydown", handleEscape);
    return () => {
      document.removeEventListener("mousedown", handlePointerDown);
      document.removeEventListener("keydown", handleEscape);
    };
  }, [controlledOpen, variant]);

  return (
    <div className={styles.container} ref={containerRef}>
      <button
        type="button"
        className={styles.trigger}
        onClick={() => setOpenState(!controlledOpen)}
        disabled={isLoading || options.length === 0}
        aria-haspopup="listbox"
        aria-expanded={controlledOpen}
        aria-label="Choose analysis vibe"
      >
        <span className={styles.triggerLabel}>
          {variant === "inline" ? "Choose a vibe" : selectedOption?.label ?? "Choose a vibe"}
        </span>
        <span className={styles.chevron} aria-hidden>⌄</span>
      </button>

      {controlledOpen && (
        <div className={variant === "inline" ? styles.overlay : styles.popover} role="listbox" aria-label="Analysis vibes">
          {variant === "inline" && (
            <div className={styles.overlayHeader}>
              <p>Choose a vibe</p>
              <button type="button" className={styles.closeButton} onClick={() => setOpenState(false)}>
                Close
              </button>
            </div>
          )}
          {options.map((vibe) => (
            <button
              key={vibe.id}
              type="button"
              className={variant === "inline"
                ? (vibe.id === mode ? styles.inlineOptionActive : styles.inlineOption)
                : (vibe.id === mode ? styles.optionActive : styles.option)}
              onClick={() => {
                setMode(vibe.id);
                setOpenState(false);
              }}
            >
              <span className={styles.optionLabel}>{variant === "inline" ? vibe.compactLabel : vibe.label}</span>
              {variant !== "inline" && <span className={styles.optionDescription}>{vibe.description}</span>}
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
