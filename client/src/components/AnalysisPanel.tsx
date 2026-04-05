import ReactMarkdown from "react-markdown";
import type { ReactNode } from "react";
import styles from "../styles/AnalysisPanel.module.css";

interface Props {
  header?: ReactNode;
  quickInsight?: string;
  quickStatsLine?: string;
  onAnalyze?: () => void;
  actionLabel?: string;
  actions?: ReactNode;
  isAnalyzing?: boolean;
  analysis?: string;
  persona?: string;
  disabled?: boolean;
  modeLabel?: string;
  dataThroughLabel?: string;
}

function AnalysisPanel({
  header,
  quickInsight,
  quickStatsLine,
  onAnalyze,
  actionLabel,
  actions,
  isAnalyzing = false,
  analysis,
  persona,
  disabled,
  modeLabel,
  dataThroughLabel,
}: Props) {
  const showAnalysisBody = Boolean(isAnalyzing || analysis);

  return (
    <section className={styles.panel} aria-label="AI analysis">
      {header}
      {quickInsight && (
        <div className={styles.quickInsight}>
          <h3>Quick insight</h3>
          <p>{quickInsight}</p>
          {quickStatsLine && <p className={styles.freshness}>{quickStatsLine}</p>}
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
            <>
              <p className={styles.loading}>Generating the latest scouting notes…</p>
              {dataThroughLabel && <p className={styles.loadingMeta}>Using data through games on {dataThroughLabel}.</p>}
            </>
          ) : (
            analysis && <ReactMarkdown>{analysis}</ReactMarkdown>
          )}
        </article>
      )}

      {actions && <div className={styles.actions}>{actions}</div>}
    </section>
  );
}

export default AnalysisPanel;
