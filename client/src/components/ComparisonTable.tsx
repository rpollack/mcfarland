import clsx from "clsx";
import styles from "../styles/ComparisonTable.module.css";
import type { HitterRecord, PitcherRecord, PlayerType } from "../types";

interface Props {
  type: PlayerType;
  players: (HitterRecord | PitcherRecord)[];
  recommendedPlayerId: string | null;
}

const hitterMetrics = [
  { key: "AVG", field: "AVG_cur", formatter: (value: number | null) => formatValue(value) },
  { key: "OBP", field: "OBP_cur", formatter: (value: number | null) => formatValue(value) },
  { key: "SLG", field: "SLG_cur", formatter: (value: number | null) => formatValue(value) },
  { key: "wOBA", field: "wOBA_cur", formatter: (value: number | null) => formatValue(value) },
  { key: "xwOBA", field: "xwOBA_cur", formatter: (value: number | null) => formatValue(value) },
  { key: "K%", field: "K_pct_cur", formatter: (value: number | null) => formatPercent(value) },
  { key: "BB%", field: "BB_pct_cur", formatter: (value: number | null) => formatPercent(value) },
  { key: "Barrel%", field: "Barrel_pct_cur", formatter: (value: number | null) => formatPercent(value) },
];

const pitcherMetrics = [
  { key: "ERA", field: "era_cur", formatter: (value: number | null) => (value === null ? "N/A" : Number(value).toFixed(2)) },
  { key: "xERA", field: "xera_cur", formatter: (value: number | null) => (value === null ? "N/A" : Number(value).toFixed(2)) },
  { key: "K%", field: "k_percent_cur", formatter: (value: number | null) => formatPercent(value) },
  { key: "BB%", field: "bb_percent_cur", formatter: (value: number | null) => formatPercent(value) },
  { key: "K-BB%", field: "k_minus_bb_percent_cur", formatter: (value: number | null) => formatPercent(value) },
  { key: "CSW%", field: "csw_percent_cur", formatter: (value: number | null) => formatPercent(value) },
  { key: "BABIP", field: "babip_cur", formatter: (value: number | null) => formatValue(value) },
  { key: "LOB%", field: "lob_percent_cur", formatter: (value: number | null) => formatPercent(value) },
];

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

function ComparisonTable({ type, players, recommendedPlayerId }: Props) {
  const metrics = type === "hitter" ? hitterMetrics : pitcherMetrics;

  return (
    <section className={styles.container} aria-label="Player comparison">
      <table className={styles.table}>
        <thead>
          <tr>
            <th scope="col">Metric</th>
            {players.map((player) => (
              <th key={player.PlayerId} scope="col" className={clsx({ [styles.highlight]: player.PlayerId === recommendedPlayerId })}>
                {player.Name}
                {player.PlayerId === recommendedPlayerId && <span className={styles.badge}>Recommended</span>}
              </th>
            ))}
          </tr>
        </thead>
        <tbody>
          {metrics.map((metric) => (
            <tr key={metric.key}>
              <th scope="row">{metric.key}</th>
              {players.map((player) => (
                <td key={`${metric.key}-${player.PlayerId}`}>{metric.formatter((player as any)[metric.field] ?? null)}</td>
              ))}
            </tr>
          ))}
        </tbody>
      </table>
    </section>
  );
}

export default ComparisonTable;
