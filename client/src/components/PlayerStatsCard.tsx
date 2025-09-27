import { Fragment } from "react";
import styles from "../styles/PlayerStatsCard.module.css";
import type { HitterRecord, PitcherRecord, PlayerType } from "../types";

type Props = {
  type: PlayerType;
  player: HitterRecord | PitcherRecord;
};

function formatValue(value: number | null, digits = 3) {
  if (value === null || Number.isNaN(value)) {
    return "N/A";
  }
  if (Math.abs(value) < 1) {
    return Number(value).toFixed(digits).replace(/^(-?)0+/, "$1");
  }
  return Number(value).toFixed(digits);
}

function formatPercent(value: number | null) {
  if (value === null || Number.isNaN(value)) {
    return "N/A";
  }
  return `${Number(value).toFixed(1)}%`.replace(/^(-?)0+([0-9])/, "$1$2");
}

const hitterMetrics = [
  { key: "AVG", cur: "AVG_cur", l3: "AVG_l3", diff: "AVG_diff", formatter: formatValue },
  { key: "OBP", cur: "OBP_cur", l3: "OBP_l3", diff: "OBP_diff", formatter: formatValue },
  { key: "SLG", cur: "SLG_cur", l3: "SLG_l3", diff: "SLG_diff", formatter: formatValue },
  { key: "K%", cur: "K_pct_cur", l3: "K_pct_l3", diff: "K_pct_diff", formatter: formatPercent },
  { key: "BB%", cur: "BB_pct_cur", l3: "BB_pct_l3", diff: "BB_pct_diff", formatter: formatPercent },
  { key: "Barrel%", cur: "Barrel_pct_cur", l3: "Barrel_pct_l3", diff: "Barrel_pct_diff", formatter: formatPercent },
  { key: "BABIP", cur: "BABIP_cur", l3: "BABIP_l3", diff: "BABIP_diff", formatter: formatValue },
  { key: "wOBA", cur: "wOBA_cur", l3: "wOBA_l3", diff: "wOBA_diff", formatter: formatValue },
  { key: "xwOBA", cur: "xwOBA_cur", l3: "xwOBA_l3", diff: "xwOBA_diff", formatter: formatValue },
  { key: "xwOBA - wOBA", cur: "xwOBA_wOBA_gap_cur", l3: "xwOBA_wOBA_gap_l3", diff: "xwOBA_wOBA_gap_diff", formatter: formatValue },
] as const;

const pitcherMetrics = [
  { key: "ERA", cur: "era_cur", l3: "era_l3", diff: "era_diff", formatter: (value: number | null) => (value === null ? "N/A" : Number(value).toFixed(2)) },
  { key: "xERA", cur: "xera_cur", l3: "xera_l3", diff: "xera_diff", formatter: (value: number | null) => (value === null ? "N/A" : Number(value).toFixed(2)) },
  { key: "BABIP", cur: "babip_cur", l3: "babip_l3", diff: "babip_diff", formatter: formatValue },
  { key: "Barrel%", cur: "barrel_percent_cur", l3: "barrel_percent_l3", diff: "barrel_percent_diff", formatter: formatPercent },
  { key: "K%", cur: "k_percent_cur", l3: "k_percent_l3", diff: "k_percent_diff", formatter: formatPercent },
  { key: "BB%", cur: "bb_percent_cur", l3: "bb_percent_l3", diff: "bb_percent_diff", formatter: formatPercent },
  { key: "K-BB%", cur: "k_minus_bb_percent_cur", l3: "k_minus_bb_percent_l3", diff: "k_minus_bb_percent_diff", formatter: formatPercent },
  { key: "CSW%", cur: "csw_percent_cur", l3: "csw_percent_l3", diff: "csw_percent_diff", formatter: formatPercent },
  { key: "O-Swing%", cur: "o_swing_percent_cur", l3: "o_swing_percent_l3", diff: "o_swing_percent_diff", formatter: formatPercent },
  { key: "LOB%", cur: "lob_percent_cur", l3: "lob_percent_l3", diff: "lob_percent_diff", formatter: formatPercent },
] as const;

function PlayerStatsCard({ type, player }: Props) {
  const metrics = type === "hitter" ? hitterMetrics : pitcherMetrics;

  return (
    <section className={styles.card} aria-label="Player statistics">
      <header className={styles.header}>
        <h2>{player.Name}</h2>
        <p>
          Age {formatValue((player as HitterRecord).Age ?? (player as PitcherRecord).Age, 1)} · {type === "hitter" ? `${formatValue((player as HitterRecord).PA_cur, 0)} PA` : `${formatValue((player as PitcherRecord).tbf, 0)} TBF`}
        </p>
      </header>
      <div className={styles.metricsGrid}>
        <div className={styles.metricHeader}>Metric</div>
        <div className={styles.metricHeader}>Current</div>
        <div className={styles.metricHeader}>Last 3 Years</div>
        <div className={styles.metricHeader}>Diff</div>
        {metrics.map((metric) => (
          <Fragment key={metric.key}>
            <div className={styles.metricLabel}>{metric.key}</div>
            <div className={styles.metricValue}>{metric.formatter((player as any)[metric.cur] ?? null)}</div>
            <div className={styles.metricValue}>{metric.formatter((player as any)[metric.l3] ?? null)}</div>
            <div className={styles.metricValue}>{metric.formatter((player as any)[metric.diff] ?? null)}</div>
          </Fragment>
        ))}
      </div>
    </section>
  );
}

export default PlayerStatsCard;
