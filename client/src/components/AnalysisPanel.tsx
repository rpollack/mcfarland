import ReactMarkdown from "react-markdown";
import styles from "../styles/AnalysisPanel.module.css";

interface Props {
  quickInsight: string;
  onAnalyze: () => void;
  isAnalyzing: boolean;
  analysis?: string;
  persona?: string;
  disabled?: boolean;
  modeLabel: string;
}

function AnalysisPanel({ quickInsight, onAnalyze, isAnalyzing, analysis, persona, disabled, modeLabel }: Props) {
  return (
    <section className={styles.panel} aria-label="AI analysis">
      <header className={styles.header}>
        <div>
          <h3>Quick Insight</h3>
          <p>{quickInsight}</p>
        </div>
        <button onClick={onAnalyze} disabled={disabled || isAnalyzing} className={styles.button}>
          {isAnalyzing ? "Analyzing..." : `Run ${modeLabel} analysis`}
        </button>
      </header>
      {analysis && (
        <article className={styles.analysis}>
          {persona && <p className={styles.persona}>Persona: {persona}</p>}
          <ReactMarkdown>{analysis}</ReactMarkdown>
        </article>
      )}
    </section>
  );
}

export default AnalysisPanel;
