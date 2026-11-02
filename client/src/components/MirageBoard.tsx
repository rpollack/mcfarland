import { useQuery } from "@tanstack/react-query";
import { fetchMirageBoard, fetchMirageBoardIntro } from "../api";
import { useVibe } from "../contexts/vibe";
import type { MirageRow, PlayerType } from "../types";
import { formatLuckGap, formatStatValue, gapBarPercent } from "../utils/keepGoing";
import PlayerHeadshot from "./PlayerHeadshot";
import styles from "../styles/MirageBoard.module.css";

interface Props {
  playerType: PlayerType;
  onSelectPlayer: (playerId: string) => void;
}

function MirageList({
  title,
  subtitle,
  rows,
  playerType,
  maxAbsGap,
  tone,
  onSelectPlayer,
}: {
  title: string;
  subtitle: string;
  rows: MirageRow[];
  playerType: PlayerType;
  maxAbsGap: number;
  tone: "lucky" | "unlucky";
  onSelectPlayer: (playerId: string) => void;
}) {
  const actualLabel = playerType === "hitter" ? "wOBA" : "ERA";

  return (
    <section className={styles.list} aria-label={title}>
      <h3 className={styles.listTitle}>{title}</h3>
      <p className={styles.listSubtitle}>{subtitle}</p>
      <ul className={styles.rows}>
        {rows.map((row) => (
          <li key={row.id}>
            <button type="button" className={styles.row} onClick={() => onSelectPlayer(row.id)}>
              <PlayerHeadshot name={row.name} mlbamid={row.mlbamid} size={32} />
              <span className={styles.rowText}>
                <span className={styles.rowName}>{row.name}</span>
                <span className={styles.rowStats}>
                  {actualLabel} {formatStatValue(row.actual, playerType)}, expected {formatStatValue(row.expected, playerType)}
                </span>
              </span>
              <span className={styles.gap}>
                <span className={styles.gapValue}>{formatLuckGap(row.gap, playerType)}</span>
                <span className={styles.track} aria-hidden="true">
                  <span
                    className={tone === "lucky" ? styles.fillLucky : styles.fillUnlucky}
                    style={{ width: `${gapBarPercent(row.gap, maxAbsGap)}%` }}
                  />
                </span>
              </span>
            </button>
          </li>
        ))}
      </ul>
    </section>
  );
}

function MirageBoard({ playerType, onSelectPlayer }: Props) {
  const { mode, vibes } = useVibe();
  const boardQuery = useQuery({
    queryKey: ["mirage-board", playerType],
    queryFn: () => fetchMirageBoard(playerType),
    staleTime: 10 * 60 * 1000,
  });
  const introQuery = useQuery({
    queryKey: ["mirage-board-intro", playerType, mode],
    queryFn: () => fetchMirageBoardIntro(playerType, mode),
    enabled: Boolean(boardQuery.data),
    staleTime: 10 * 60 * 1000,
    retry: false,
  });

  const board = boardQuery.data;
  if (!board || (board.lucky.length === 0 && board.unlucky.length === 0)) {
    return null;
  }

  const vibeLabel = (vibes.find((vibe) => vibe.id === mode)?.label ?? "").replace(/\s*\(.*\)\s*$/, "");
  const maxAbsGap = Math.max(...[...board.lucky, ...board.unlucky].map((row) => Math.abs(row.gap)));
  const group = playerType === "hitter" ? "hitters" : "pitchers";
  const intro = introQuery.data;

  return (
    <section className={styles.board} aria-label="Mirage board">
      <header className={styles.header}>
        <h2 className={styles.title}>{board.season} mirages</h2>
        <p className={styles.subtitle}>
          The {group} whose {board.season} results ran furthest from what their skills earned.
        </p>
      </header>

      {introQuery.isLoading && (
        <div className={styles.intro} aria-busy="true">
          <div className={styles.skeleton} />
          <div className={styles.skeleton} />
          <div className={`${styles.skeleton} ${styles.skeletonShort}`} />
        </div>
      )}
      {intro && !introQuery.isError && (
        <article className={styles.intro}>
          <h3 className={styles.introHeadline}>{intro.headline}</h3>
          {intro.analysis
            .split(/\n{2,}/)
            .filter((paragraph) => paragraph.trim().length > 0)
            .map((paragraph) => (
              <p key={paragraph} className={styles.introParagraph}>
                {paragraph}
              </p>
            ))}
          {vibeLabel && <p className={styles.byline}>Written in the {vibeLabel} voice.</p>}
        </article>
      )}

      <div className={styles.columns}>
        <MirageList
          title="Ran lucky"
          subtitle="Results ahead of the skills"
          rows={board.lucky}
          playerType={playerType}
          maxAbsGap={maxAbsGap}
          tone="lucky"
          onSelectPlayer={onSelectPlayer}
        />
        <MirageList
          title="Ran unlucky"
          subtitle="Results behind the skills"
          rows={board.unlucky}
          playerType={playerType}
          maxAbsGap={maxAbsGap}
          tone="unlucky"
          onSelectPlayer={onSelectPlayer}
        />
      </div>

      <p className={styles.hint}>
        {playerType === "hitter"
          ? "Gap is expected wOBA minus actual wOBA. Positive means the results trail the contact quality."
          : "Gap is ERA minus expected ERA. Positive means the results are worse than the underlying skills."}{" "}
        Tap a player for the full take.
      </p>
    </section>
  );
}

export default MirageBoard;
