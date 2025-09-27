import { useState } from "react";
import SinglePlayerPage from "./pages/SinglePlayerPage";
import ComparePage from "./pages/ComparePage";
import AboutPage from "./pages/AboutPage";
import { VibeProvider } from "./contexts/VibeContext";
import styles from "./styles/App.module.css";

type ExperienceMode = "single" | "compare";
type View = "experience" | "about";

function App() {
  const [mode, setMode] = useState<ExperienceMode>("single");
  const [view, setView] = useState<View>("experience");

  return (
    <VibeProvider>
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
              aria-selected={mode === "single"}
              className={mode === "single" ? styles.activeTab : styles.tab}
              onClick={() => setMode("single")}
            >
              Single Player
            </button>
            <button
              type="button"
              role="tab"
              aria-selected={mode === "compare"}
              className={mode === "compare" ? styles.activeTab : styles.tab}
              onClick={() => setMode("compare")}
            >
              Compare Players
            </button>
          </div>
        )}

        <main className={styles.main}>
          {view === "about" ? (
            <AboutPage />
          ) : mode === "single" ? (
            <SinglePlayerPage />
          ) : (
            <ComparePage />
          )}
        </main>
      </div>
    </VibeProvider>
  );
}

export default App;
