import { useCallback, useEffect, useMemo, useState } from "react";
import { useSearchParams } from "react-router-dom";
import SinglePlayerPage from "./pages/SinglePlayerPage";
import ComparePage from "./pages/ComparePage";
import AboutPage from "./pages/AboutPage";
import { VibeProvider, useVibe } from "./contexts/VibeContext";
import styles from "./styles/App.module.css";
import { registerSession } from "./api";
import type { PlayerType } from "./types";

type ExperienceMode = "single" | "compare";
type View = "experience" | "about";

const EXPERIENCE_PARAM = "mode";
const PLAYER_TYPE_PARAM = "playerType";
const PLAYER_ID_PARAM = "playerId";
const PLAYER_IDS_PARAM = "playerIds";
const VIBE_PARAM = "vibe";

function AppShell() {
  const [searchParams, setSearchParams] = useSearchParams();
  const { mode: vibeMode, setMode: setVibeMode, vibes } = useVibe();

  const urlMode = searchParams.get(EXPERIENCE_PARAM) === "compare" ? "compare" : "single";
  const [experienceMode, setExperienceMode] = useState<ExperienceMode>(urlMode);
  const [view, setView] = useState<View>("experience");

  useEffect(() => {
    setExperienceMode(urlMode);
  }, [urlMode]);

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
      setSearchParams(next, { replace: true });
    },
    [searchParams, setSearchParams]
  );

  const playerTypeParam: PlayerType = searchParams.get(PLAYER_TYPE_PARAM) === "pitcher" ? "pitcher" : "hitter";
  const singlePlayerId = searchParams.get(PLAYER_ID_PARAM) ?? undefined;
  const comparePlayerIds = useMemo(
    () => searchParams.get(PLAYER_IDS_PARAM)?.split(",").map((id) => id.trim()).filter(Boolean).slice(0, 3) ?? [],
    [searchParams]
  );

  useEffect(() => {
    const param = searchParams.get(VIBE_PARAM);
    if (!param) {
      return;
    }
    if (param !== vibeMode && (vibes.length === 0 || vibes.some((vibe) => vibe.id === param))) {
      setVibeMode(param);
    }
  }, [searchParams, vibeMode, setVibeMode, vibes]);

  useEffect(() => {
    if (!vibeMode) {
      return;
    }
    if (searchParams.get(VIBE_PARAM) !== vibeMode) {
      applySearchParams({ [VIBE_PARAM]: vibeMode });
    }
  }, [vibeMode, searchParams, applySearchParams]);

  useEffect(() => {
    if (!searchParams.get(EXPERIENCE_PARAM)) {
      applySearchParams({ [EXPERIENCE_PARAM]: experienceMode });
    }
  }, [experienceMode, searchParams, applySearchParams]);

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

  return (
    <div className={styles.shell}>
      <header className={styles.hero}>
        <div>
          <h1 className={styles.title}>McFarland</h1>
          <p className={styles.tagline}>Advanced baseball analysis. Plain English.</p>
        </div>
        <div className={styles.heroActions}>
          {view === "experience" ? (
            <button type="button" className={styles.linkButton} onClick={() => setView("about")}>
              About McFarland
            </button>
          ) : (
            <button type="button" className={styles.linkButton} onClick={() => setView("experience")}>
              ← Back to analysis
            </button>
          )}
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
        {view === "about" ? (
          <AboutPage />
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
