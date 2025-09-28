import ReactMarkdown from "react-markdown";
import styles from "../styles/AnalysisPanel.module.css";

interface Props {
  quickInsight?: string;
  onAnalyze?: () => void;
  actionLabel?: string;
  isAnalyzing?: boolean;
  analysis?: string;
  persona?: string;
  disabled?: boolean;
  modeLabel?: string;
}

function AnalysisPanel({
  quickInsight,
  onAnalyze,
  actionLabel,
  isAnalyzing = false,
  analysis,
  persona,
  disabled,
  modeLabel,
}: Props) {
  const showAnalysisBody = Boolean(isAnalyzing || analysis);

  return (
    <section className={styles.panel} aria-label="AI analysis">
      {quickInsight && (
        <div className={styles.quickInsight}>
          <h3>Quick insight</h3>
          <p>{quickInsight}</p>
        </div>
      )}

      {onAnalyze && (
        <button
          type="button"
          onClick={onAnalyze}
          disabled={disabled || isAnalyzing}
          className={styles.button}
        >
          {isAnalyzing ? "Analyzing…" : actionLabel ?? `Run ${modeLabel ?? "AI"} analysis`}
        </button>
      )}

      {showAnalysisBody && (
        <article className={styles.analysis} aria-live="polite">
          {isAnalyzing ? (
            <p className={styles.loading}>Generating the latest scouting notes…</p>
          ) : (
            analysis && <ReactMarkdown>{analysis}</ReactMarkdown>
          )}
        </article>
      )}
    </section>
  );
}

export default AnalysisPanel;
