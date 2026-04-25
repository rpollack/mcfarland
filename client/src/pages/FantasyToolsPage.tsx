import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { useMutation, useQuery } from "@tanstack/react-query";
import ReactMarkdown from "react-markdown";
import { analyzeDailyMatchup, fetchPlayers } from "../api";
import PlayerHeadshot from "../components/PlayerHeadshot";
import PlayerPicker from "../components/PlayerPicker";
import type { DailyMatchupContext, FantasyDailyMatchupResponse, PlayerRecord, PlayerType } from "../types";
import styles from "../styles/FantasyToolsPage.module.css";

interface Props {
  initialPlayerType: PlayerType;
  initialPlayerId?: string;
  onStateChange: (state: { playerType: PlayerType; playerId?: string }) => void;
}

const SEARCH_MIN_LENGTH = 2;
const SEARCH_RESULT_LIMIT = 8;

type PlatoonAdvantage = {
  label: "Yes" | "No" | "Unknown";
  className: string;
  icon: string;
  explanation: string;
};

function formatGameTime(value?: string): string {
  if (!value) {
    return "TBD";
  }
  const date = new Date(value);
  if (Number.isNaN(date.getTime())) {
    return value;
  }
  return new Intl.DateTimeFormat("en-US", {
    weekday: "short",
    hour: "numeric",
    minute: "2-digit",
    timeZoneName: "short",
  }).format(date);
}

function formatOrdinalDay(day: number): string {
  if (day >= 11 && day <= 13) {
    return `${day}th`;
  }
  const lastDigit = day % 10;
  if (lastDigit === 1) return `${day}st`;
  if (lastDigit === 2) return `${day}nd`;
  if (lastDigit === 3) return `${day}rd`;
  return `${day}th`;
}

function formatDateLabel(date: Date, timeZone = "America/Chicago"): string {
  const parts = new Intl.DateTimeFormat("en-US", {
    weekday: "long",
    month: "long",
    day: "numeric",
    timeZone,
  }).formatToParts(date);
  const weekday = parts.find((part) => part.type === "weekday")?.value;
  const month = parts.find((part) => part.type === "month")?.value;
  const day = Number(parts.find((part) => part.type === "day")?.value);

  if (!weekday || !month || !Number.isFinite(day)) {
    return "Today";
  }
  return `${weekday}, ${month} ${formatOrdinalDay(day)}`;
}

function formatMatchupDate(matchup: DailyMatchupContext): string {
  const rawDate = matchup.gameDate ?? matchup.date;
  if (!rawDate) {
    return "Today";
  }
  const date = new Date(rawDate.includes("T") ? rawDate : `${rawDate}T12:00:00`);
  if (Number.isNaN(date.getTime())) {
    return rawDate;
  }
  return formatDateLabel(date);
}

function formatLineupStatus(matchup: DailyMatchupContext): string {
  if (matchup.lineupStatus === "confirmed") return "Confirmed in lineup";
  if (matchup.lineupStatus === "notInLineup") return "Not in posted lineup";
  if (matchup.lineupStatus === "unavailable") return "Lineup not posted";
  return "N/A";
}

function formatWeather(matchup: DailyMatchupContext): string {
  if (!matchup.weather) {
    return "N/A";
  }
  return [matchup.weather.condition, matchup.weather.temp ? `${matchup.weather.temp} degrees` : null, matchup.weather.wind]
    .filter(Boolean)
    .join(" · ") || "N/A";
}

function getPlatoonAdvantage(matchup: DailyMatchupContext): PlatoonAdvantage {
  const batSide = matchup.selectedPlayerHandedness?.batSide;
  const pitchHand = matchup.opposingStarter?.pitchHand;
  const explanation = matchup.platoonLabel ?? "Handedness matchup unavailable";

  if (!batSide || !pitchHand) {
    return {
      label: "Unknown",
      className: styles.platoonUnknown,
      icon: "-",
      explanation,
    };
  }

  const hasAdvantage = batSide === "S" || batSide !== pitchHand;
  return {
    label: hasAdvantage ? "Yes" : "No",
    className: hasAdvantage ? styles.platoonYes : styles.platoonNo,
    icon: hasAdvantage ? "✓" : "✕",
    explanation,
  };
}

function MatchupCard({ matchup, playerType }: { matchup: DailyMatchupContext; playerType: PlayerType }) {
  const matchupDate = formatMatchupDate(matchup);

  if (matchup.matchupStatus === "missing_mlbam_id") {
    return (
      <section className={styles.card} aria-label="Daily matchup">
        <h3>Game Summary</h3>
        <p className={styles.cardDate}>{matchupDate}</p>
        <p className={styles.muted}>This player is missing an MLBAM ID, so live matchup context is unavailable.</p>
      </section>
    );
  }

  if (matchup.matchupStatus === "no_game") {
    return (
      <section className={styles.card} aria-label="Daily matchup">
        <h3>Game Summary</h3>
        <p className={styles.cardDate}>{matchupDate}</p>
        <p className={styles.muted}>No MLB game found for this player today.</p>
      </section>
    );
  }

  const platoonAdvantage = getPlatoonAdvantage(matchup);

  return (
    <section className={styles.card} aria-label="Daily matchup">
      <div className={styles.cardHeader}>
        <div>
          <h3>Game Summary</h3>
          <p className={styles.cardDate}>{matchupDate}</p>
        </div>
        <span className={styles.statusPill}>{matchup.gameStatus ?? "Scheduled"}</span>
      </div>
      <dl className={styles.facts}>
        <div>
          <dt>Game</dt>
          <dd>
            {matchup.playerTeam?.abbreviation ?? matchup.playerTeam?.name ?? "Team"} {matchup.homeAway === "away" ? "at" : "vs"}{" "}
            {matchup.opponent?.abbreviation ?? matchup.opponent?.name ?? "Opponent"}
          </dd>
        </div>
        <div>
          <dt>Time</dt>
          <dd>{formatGameTime(matchup.gameDate)}</dd>
        </div>
        <div>
          <dt>Venue</dt>
          <dd>{matchup.venue ?? "N/A"}</dd>
        </div>
        <div>
          <dt>Weather</dt>
          <dd>{formatWeather(matchup)}</dd>
        </div>
        <div>
          <dt>{playerType === "hitter" ? "Opposing starter" : "Team starter"}</dt>
          <dd>
            {playerType === "hitter"
              ? `${matchup.opposingStarter?.name ?? "Not posted"}${matchup.opposingStarter?.pitchHand ? ` (${matchup.opposingStarter.pitchHand}HP)` : ""}`
              : `${matchup.selectedTeamStarter?.name ?? "Not posted"}${matchup.isProbableStarter === false ? " · selected pitcher not listed" : ""}`}
          </dd>
        </div>
        <div>
          <dt>{playerType === "hitter" ? "Platoon advantage" : "Opponent starter"}</dt>
          <dd>
            {playerType === "hitter" ? (
              <span className={styles.platoonSummary}>
                <span className={`${styles.platoonBadge} ${platoonAdvantage.className}`}>
                  {platoonAdvantage.icon} {platoonAdvantage.label}
                </span>
                <span className={styles.platoonExplanation}>{platoonAdvantage.explanation}</span>
              </span>
            ) : (
              `${matchup.opposingStarter?.name ?? "Not posted"}${matchup.opposingStarter?.pitchHand ? ` (${matchup.opposingStarter.pitchHand}HP)` : ""}`
            )}
          </dd>
        </div>
        {playerType === "hitter" && (
          <div>
            <dt>Lineup</dt>
            <dd>{formatLineupStatus(matchup)}</dd>
          </div>
        )}
      </dl>
    </section>
  );
}

function FantasyToolsPage({ initialPlayerType, initialPlayerId, onStateChange }: Props) {
  const [playerType, setPlayerType] = useState<PlayerType>(initialPlayerType);
  const [searchTerm, setSearchTerm] = useState("");
  const [selectedId, setSelectedId] = useState<string | undefined>(initialPlayerId);
  const syncingFromPropsRef = useRef(false);
  const lastRequestKeyRef = useRef<string | null>(null);

  useEffect(() => {
    syncingFromPropsRef.current = true;
    setPlayerType(initialPlayerType);
    setSelectedId(initialPlayerId);
    if (initialPlayerId) {
      setSearchTerm("");
    }
  }, [initialPlayerType, initialPlayerId]);

  const trimmedSearch = searchTerm.trim();
  const searchEnabled = trimmedSearch.length >= SEARCH_MIN_LENGTH;

  const playersQuery = useQuery({
    queryKey: ["fantasy-players", playerType, trimmedSearch],
    queryFn: () => fetchPlayers(playerType, trimmedSearch, SEARCH_RESULT_LIMIT),
    enabled: searchEnabled,
  });

  const {
    mutate: runDailyMatchup,
    data,
    isPending,
    isError,
    reset,
  } = useMutation({
    mutationFn: ({ playerId, type }: { playerId: string; type: PlayerType }) =>
      analyzeDailyMatchup<PlayerRecord>(playerId, type),
    onError: () => {
      lastRequestKeyRef.current = null;
    },
    retry: false,
  });

  useEffect(() => {
    if (!selectedId) {
      lastRequestKeyRef.current = null;
      reset();
      return;
    }
    const key = `${playerType}|${selectedId}`;
    if (lastRequestKeyRef.current === key) {
      return;
    }
    lastRequestKeyRef.current = key;
    runDailyMatchup({ playerId: selectedId, type: playerType });
  }, [playerType, selectedId, runDailyMatchup, reset]);

  useEffect(() => {
    if (syncingFromPropsRef.current) {
      syncingFromPropsRef.current = false;
      return;
    }
    onStateChange({ playerType, playerId: selectedId });
  }, [playerType, selectedId, onStateChange]);

  const availablePlayers = useMemo(() => (searchEnabled ? playersQuery.data ?? [] : []), [playersQuery.data, searchEnabled]);

  const handleTypeChange = useCallback((nextType: PlayerType) => {
    setPlayerType(nextType);
    setSelectedId(undefined);
    setSearchTerm("");
    reset();
  }, [reset]);

  const result = data as FantasyDailyMatchupResponse<PlayerRecord> | undefined;
  const playerName = result?.player.Name;
  const todayLabel = useMemo(() => formatDateLabel(new Date()), []);

  return (
    <div className={styles.container}>
      <section className={styles.selectionPanel} aria-label="Fantasy daily matchup tool">
        <header className={styles.toolHeader}>
          <h2>Daily Matchup: {todayLabel}</h2>
          <p>Pick one player and get a clear start/sit recommendation using today&apos;s MLB matchup context.</p>
        </header>
        <PlayerPicker
          embedded
          playerType={playerType}
          onTypeChange={handleTypeChange}
          searchTerm={searchTerm}
          onSearchTermChange={(value) => {
            setSearchTerm(value);
            if (value.trim().length > 0) {
              setSelectedId(undefined);
            }
          }}
          players={availablePlayers}
          selectedId={selectedId}
          onSelect={setSelectedId}
          isLoading={searchEnabled && playersQuery.isLoading}
        />
      </section>

      {selectedId && (
        <div className={styles.results}>
          {isPending && (
            <div className={styles.loadingCard}>
              <p>Building the fantasy recommendation ...</p>
            </div>
          )}
          {isError && !isPending && (
            <div className={styles.loadingCard}>
              <p>Couldn&apos;t load the daily matchup. Try another player or check back shortly.</p>
            </div>
          )}
          {result && !isPending && (
            <>
              <section className={styles.identityCard}>
                <PlayerHeadshot
                  name={result.player.Name}
                  playerId={result.player.PlayerId}
                  mlbamid={result.player.mlbamid}
                  size={56}
                  className={styles.headshot}
                />
                <div>
                  <h2>{playerName}</h2>
                  <p>{playerType === "hitter" ? "Hitter" : "Pitcher"} daily fantasy decision</p>
                </div>
              </section>

              <section className={result.decision === "START" ? styles.startCard : styles.sitCard} aria-label="Start sit decision">
                <div className={styles.decisionRow}>
                  <span className={styles.decision}>{result.decision}</span>
                  <span className={styles.confidence}>{result.confidence} confidence</span>
                </div>
                <h3>{result.headline}</h3>
                <ReactMarkdown>{result.analysis}</ReactMarkdown>
              </section>

              <MatchupCard matchup={result.matchup} playerType={playerType} />
            </>
          )}
        </div>
      )}
    </div>
  );
}

export default FantasyToolsPage;
