import { useState } from "react";
import SinglePlayerPage from "./pages/SinglePlayerPage";
import ComparePage from "./pages/ComparePage";
import { VibeProvider } from "./contexts/VibeContext";
import styles from "./styles/App.module.css";

type ExperienceMode = "single" | "compare";

function App() {
  const [mode, setMode] = useState<ExperienceMode>("single");

  return (
    <VibeProvider>
      <div className={styles.shell}>
        <header className={styles.hero}>
          <div>
            <h1 className={styles.title}>McFarland</h1>
            <p className={styles.tagline}>Modern baseball intelligence</p>
          </div>
        </header>

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

        <main className={styles.main}>
          {mode === "single" ? <SinglePlayerPage /> : <ComparePage />}
        </main>
      </div>
    </VibeProvider>
  );
}

export default App;
