import ReactMarkdown from "react-markdown";
import type { ReactNode } from "react";
import styles from "../styles/AnalysisPanel.module.css";

export type QuickStatSignal = {
  label: string;
  tone?: "positive" | "negative" | "neutral";
};

interface Props {
  header?: ReactNode;
  quickInsight?: string;
  quickStatsLine?: string;
  quickStatSignals?: QuickStatSignal[];
  onAnalyze?: () => void;
  actionLabel?: string;
  nextSteps?: ReactNode;
  isAnalyzing?: boolean;
  headline?: string;
  analysis?: string;
  persona?: string;
  disabled?: boolean;
  modeLabel?: string;
}

function AnalysisPanel({
  header,
  quickInsight,
  quickStatsLine,
  quickStatSignals = [],
  onAnalyze,
  actionLabel,
  nextSteps,
  isAnalyzing = false,
  headline,
  analysis,
  persona,
  disabled,
  modeLabel,
}: Props) {
  const showAnalysisBody = Boolean(isAnalyzing || analysis);
  const normalizedHeadline = headline?.replace(/\s+/g, " ").trim();
  const showContext = Boolean(quickStatsLine || quickStatSignals.length > 0);

  return (
    <section className={styles.panel} aria-label="AI analysis">
      {header}
      {!showAnalysisBody && quickInsight && <p className={styles.preAnalysisHint}>{quickInsight}</p>}

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
            analysis && (
              <>
                {normalizedHeadline && <h3 className={styles.headline}>{normalizedHeadline}</h3>}
                {showContext && (
                  <div className={styles.analysisContext}>
                    {quickStatsLine && <p className={styles.freshness}>{quickStatsLine}</p>}
                    {quickStatSignals.length > 0 && (
                      <div className={styles.signalChips} aria-label="Key signals">
                        {quickStatSignals.map((signal) => (
                          <span
                            key={signal.label}
                            className={`${styles.signalChip} ${styles[signal.tone ?? "neutral"]}`}
                          >
                            {signal.label}
                          </span>
                        ))}
                      </div>
                    )}
                  </div>
                )}
                <ReactMarkdown>{analysis}</ReactMarkdown>
              </>
            )
          )}
        </article>
      )}

      {nextSteps && (
        <section className={styles.nextSteps} aria-label="Next steps">
          <div className={styles.nextStepsHeader}>
            <h3>Next steps</h3>
          </div>
          <div className={styles.nextStepsGrid}>{nextSteps}</div>
        </section>
      )}
    </section>
  );
}

export default AnalysisPanel;
