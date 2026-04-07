import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { useSearchParams } from "react-router-dom";
import SinglePlayerPage from "./pages/SinglePlayerPage";
import ComparePage from "./pages/ComparePage";
import AboutPage from "./pages/AboutPage";
import SocialAssistantPage from "./pages/SocialAssistantPage";
import { VibeProvider, useVibe } from "./contexts/VibeContext";
import styles from "./styles/App.module.css";
import { registerSession } from "./api";
import type { PlayerType } from "./types";

type ExperienceMode = "single" | "compare";
type View = "experience" | "about" | "social";

const EXPERIENCE_PARAM = "mode";
const PLAYER_TYPE_PARAM = "playerType";
const PLAYER_ID_PARAM = "playerId";
const PLAYER_IDS_PARAM = "playerIds";
const VIBE_PARAM = "vibe";
const TOOL_PARAM = "tool";

function normalizeSearchParams(params: URLSearchParams, defaultMode: string): URLSearchParams {
  const next = new URLSearchParams(params);
  const experienceMode = next.get(EXPERIENCE_PARAM);
  const playerType = next.get(PLAYER_TYPE_PARAM);
  const playerId = next.get(PLAYER_ID_PARAM);
  const playerIds = next.get(PLAYER_IDS_PARAM);
  const vibe = next.get(VIBE_PARAM);

  if (experienceMode === "single") {
    next.delete(EXPERIENCE_PARAM);
  }

  if (vibe === defaultMode) {
    next.delete(VIBE_PARAM);
  }

  if (playerType === "hitter" && !playerId && !playerIds) {
    next.delete(PLAYER_TYPE_PARAM);
  }

  return next;
}

function AppShell() {
  const [searchParams, setSearchParams] = useSearchParams();
  const { mode: vibeMode, setMode: setVibeMode, vibes, defaultMode } = useVibe();
  const lastUrlVibeRef = useRef<string | null>(searchParams.get(VIBE_PARAM));

  const urlMode = searchParams.get(EXPERIENCE_PARAM) === "compare" ? "compare" : "single";
  const [experienceMode, setExperienceMode] = useState<ExperienceMode>(urlMode);
  const [view, setView] = useState<View>(searchParams.get(TOOL_PARAM) === "social" ? "social" : "experience");

  useEffect(() => {
    setExperienceMode(urlMode);
  }, [urlMode]);

  useEffect(() => {
    setView(searchParams.get(TOOL_PARAM) === "social" ? "social" : "experience");
  }, [searchParams]);

  const applySearchParams = useCallback(
    (updates: Record<string, string | null | undefined>) => {
      const next = new URLSearchParams(searchParams);
      Object.entries(updates).forEach(([key, value]) => {
        if (value === undefined || value === null || value === "") {
          next.delete(key);
        } else {
          next.set(key, value);
        }
      });
      const normalizedNext = normalizeSearchParams(next, defaultMode);
      const normalizedCurrent = normalizeSearchParams(searchParams, defaultMode);
      if (normalizedNext.toString() === normalizedCurrent.toString()) {
        return;
      }
      setSearchParams(normalizedNext, { replace: true });
    },
    [defaultMode, searchParams, setSearchParams]
  );

  const playerTypeParam: PlayerType = searchParams.get(PLAYER_TYPE_PARAM) === "pitcher" ? "pitcher" : "hitter";
  const singlePlayerId = searchParams.get(PLAYER_ID_PARAM) ?? undefined;
  const comparePlayerIds = useMemo(
    () => searchParams.get(PLAYER_IDS_PARAM)?.split(",").map((id) => id.trim()).filter(Boolean).slice(0, 3) ?? [],
    [searchParams]
  );

  useEffect(() => {
    const param = searchParams.get(VIBE_PARAM);
    if (!param || param === lastUrlVibeRef.current) {
      return;
    }
    lastUrlVibeRef.current = param;
    if (param !== vibeMode && (vibes.length === 0 || vibes.some((vibe) => vibe.id === param))) {
      setVibeMode(param);
    }
  }, [searchParams, vibeMode, setVibeMode, vibes]);

  useEffect(() => {
    if (!vibeMode) {
      return;
    }
    if (searchParams.get(VIBE_PARAM) !== vibeMode && vibeMode !== defaultMode) {
      lastUrlVibeRef.current = vibeMode;
      applySearchParams({ [VIBE_PARAM]: vibeMode });
    }
    if (searchParams.get(VIBE_PARAM) && vibeMode === defaultMode) {
      applySearchParams({ [VIBE_PARAM]: null });
    }
  }, [applySearchParams, defaultMode, searchParams, vibeMode]);

  const handleExperienceToggle = useCallback(
    (nextMode: ExperienceMode) => {
      setExperienceMode(nextMode);
      applySearchParams({
        [EXPERIENCE_PARAM]: nextMode,
        [PLAYER_ID_PARAM]: nextMode === "single" ? searchParams.get(PLAYER_ID_PARAM) ?? undefined : null,
        [PLAYER_IDS_PARAM]: nextMode === "compare" ? searchParams.get(PLAYER_IDS_PARAM) ?? undefined : null,
      });
    },
    [applySearchParams, searchParams]
  );

  const handleSingleStateChange = useCallback(
    ({ playerType, playerId }: { playerType: PlayerType; playerId?: string }) => {
      setExperienceMode("single");
      applySearchParams({
        [EXPERIENCE_PARAM]: "single",
        [PLAYER_TYPE_PARAM]: playerType,
        [PLAYER_ID_PARAM]: playerId ?? null,
        [PLAYER_IDS_PARAM]: null,
      });
    },
    [applySearchParams]
  );

  const handleCompareStateChange = useCallback(
    ({ playerType, playerIds }: { playerType: PlayerType; playerIds: string[] }) => {
      setExperienceMode("compare");
      const sanitized = playerIds.filter(Boolean).slice(0, 3);
      applySearchParams({
        [EXPERIENCE_PARAM]: "compare",
        [PLAYER_TYPE_PARAM]: playerType,
        [PLAYER_IDS_PARAM]: sanitized.length > 0 ? sanitized.join(",") : null,
        [PLAYER_ID_PARAM]: null,
      });
    },
    [applySearchParams]
  );

  useEffect(() => {
    if (view !== "about") {
      return;
    }
    const handleKeyDown = (event: KeyboardEvent) => {
      if (event.key === "Escape") {
        setView("experience");
      }
    };
    window.addEventListener("keydown", handleKeyDown);
    return () => {
      window.removeEventListener("keydown", handleKeyDown);
    };
  }, [view]);

  return (
    <div className={styles.shell}>
      <header className={styles.hero}>
        <div>
          <h1 className={styles.title}>
            {view === "social" ? "McFARLAND Social Assistant" : "McFARLAND: Baseball Analysis in Plain English"}
          </h1>
        </div>
      </header>

      {view === "experience" && (
        <div className={styles.switcher} role="tablist" aria-label="Analysis mode">
          <button
            type="button"
            role="tab"
            aria-selected={experienceMode === "single"}
            className={experienceMode === "single" ? styles.activeTab : styles.tab}
            onClick={() => handleExperienceToggle("single")}
          >
            Single Player
          </button>
          <button
            type="button"
            role="tab"
            aria-selected={experienceMode === "compare"}
            className={experienceMode === "compare" ? styles.activeTab : styles.tab}
            onClick={() => handleExperienceToggle("compare")}
          >
            Compare Players
          </button>
        </div>
      )}

      <main className={styles.main}>
        {view === "social" ? (
          <SocialAssistantPage />
        ) : experienceMode === "single" ? (
          <SinglePlayerPage
            initialPlayerType={playerTypeParam}
            initialPlayerId={singlePlayerId}
            onStateChange={handleSingleStateChange}
          />
        ) : (
          <ComparePage
            initialPlayerType={playerTypeParam}
            initialPlayerIds={comparePlayerIds}
            onStateChange={handleCompareStateChange}
          />
        )}
      </main>

      <footer className={styles.footer}>
        <button
          type="button"
          className={styles.linkButton}
          onClick={() => {
            if (view === "social") {
              applySearchParams({ [TOOL_PARAM]: null });
              return;
            }
            setView(view === "experience" ? "about" : "experience");
          }}
        >
          {view === "social" ? "← Back to analysis" : view === "experience" ? "About McFARLAND" : "← Back to analysis"}
        </button>
      </footer>

      {view === "about" && (
        <div className={styles.aboutOverlay}>
          <div className={styles.aboutDialog} role="dialog" aria-modal="true" aria-label="About McFARLAND">
            <button
              type="button"
              className={styles.closeButton}
              onClick={() => setView("experience")}
            >
              <span aria-hidden="true">×</span>
              <span className={styles.srOnly}>Close about panel</span>
            </button>
            <AboutPage />
          </div>
        </div>
      )}
    </div>
  );
}

function App() {
  const [searchParams] = useSearchParams();
  const initialVibeParam = searchParams.get(VIBE_PARAM);

  useEffect(() => {
    void registerSession();
  }, []);

  return (
    <VibeProvider initialMode={initialVibeParam}>
      <AppShell />
    </VibeProvider>
  );
}

export default App;
