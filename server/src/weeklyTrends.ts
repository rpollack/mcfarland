import { Pool } from "pg";
import { listPlayers } from "./dataStore.js";
import { HitterRecord, PitcherRecord } from "./types.js";

export type TrendPlayer = {
  id: string;
  name: string;
  type: "hitter" | "pitcher";
  mlbamid?: string | null;
};

export type WeeklyTrends = {
  generatedAt: string;
  weekStart: string;
  hitters: {
    risers: TrendPlayer[];
    fallers: TrendPlayer[];
  };
  pitchers: {
    risers: TrendPlayer[];
    fallers: TrendPlayer[];
  };
};

const DEFAULT_TIMEZONE = process.env.TRENDS_TIMEZONE?.trim() || "America/Chicago";

let poolPromise: Promise<Pool | null> | null = null;
let initialized = false;

type PlayerScore = TrendPlayer & { score: number };
type RankedPlayer = TrendPlayer & { momentum: number };

function toFinite(value: number | null | undefined): number {
  return typeof value === "number" && Number.isFinite(value) ? value : 0;
}

function scoreHitter(player: HitterRecord): number | null {
  if ((player.PA_cur ?? 0) < 15) {
    return null;
  }

  return (
    toFinite(player.wOBA_diff) * 520 +
    toFinite(player.xwOBA_diff) * 340 +
    toFinite(player.SLG_diff) * 170 +
    toFinite(player.OBP_diff) * 140 +
    toFinite(player.AVG_diff) * 230 +
    toFinite(player.Barrel_pct_diff) * 3.4 +
    toFinite(player.BB_pct_diff) * 1.8 -
    toFinite(player.K_pct_diff) * 1.6
  );
}

function scorePitcher(player: PitcherRecord): number | null {
  if ((player.tbf ?? 0) < 20) {
    return null;
  }

  return (
    -toFinite(player.era_diff) * 17 +
    -toFinite(player.xera_diff) * 12 +
    toFinite(player.k_minus_bb_percent_diff) * 1.9 +
    toFinite(player.k_percent_diff) * 1.1 -
    toFinite(player.bb_percent_diff) * 1.2 -
    toFinite(player.barrel_percent_diff) * 1.4 +
    toFinite(player.csw_percent_diff) * 0.9 -
    toFinite(player.babip_diff) * 14
  );
}

function getCurrentScores(type: "hitter" | "pitcher"): PlayerScore[] {
  if (type === "hitter") {
    return listPlayers("hitter")
      .map((entry) => entry as HitterRecord)
      .flatMap((player) => {
        const score = scoreHitter(player);
        if (score === null) {
          return [];
        }
        return [{
          id: player.PlayerId,
          name: player.Name,
          type: "hitter" as const,
          mlbamid: player.mlbamid ?? null,
          score,
        }];
      });
  }

  return listPlayers("pitcher")
    .map((entry) => entry as PitcherRecord)
    .flatMap((player) => {
      const score = scorePitcher(player);
      if (score === null) {
        return [];
      }
      return [{
        id: player.PlayerId,
        name: player.Name,
        type: "pitcher" as const,
        mlbamid: player.mlbamid ?? null,
        score,
      }];
    });
}

function parseLocalDateParts(date: Date, timeZone: string): { year: number; month: number; day: number; weekday: string } {
  const parts = new Intl.DateTimeFormat("en-US", {
    timeZone,
    year: "numeric",
    month: "2-digit",
    day: "2-digit",
    weekday: "short",
  }).formatToParts(date);

  const pick = (type: Intl.DateTimeFormatPartTypes): string => parts.find((part) => part.type === type)?.value ?? "";
  return {
    year: Number(pick("year")),
    month: Number(pick("month")),
    day: Number(pick("day")),
    weekday: pick("weekday"),
  };
}

function getLocalDate(date: Date, timeZone: string): string {
  const { year, month, day } = parseLocalDateParts(date, timeZone);
  return `${year}-${String(month).padStart(2, "0")}-${String(day).padStart(2, "0")}`;
}

function getWeekStartFromDateString(localDate: string): string {
  const utcDate = Date.parse(`${localDate}T00:00:00.000Z`);
  const jsDay = new Date(utcDate).getUTCDay(); // 0 Sunday ... 6 Saturday
  const mondayOffset = (jsDay + 6) % 7;
  const mondayUtc = utcDate - mondayOffset * 24 * 60 * 60 * 1000;
  return new Date(mondayUtc).toISOString().slice(0, 10);
}

function shiftDate(dateStr: string, deltaDays: number): string {
  const utcDate = Date.parse(`${dateStr}T00:00:00.000Z`);
  return new Date(utcDate + deltaDays * 24 * 60 * 60 * 1000).toISOString().slice(0, 10);
}

function inRange(dateStr: string, startInclusive: string, endInclusive: string): boolean {
  return dateStr >= startInclusive && dateStr <= endInclusive;
}

function avg(values: number[]): number | null {
  if (values.length === 0) {
    return null;
  }
  return values.reduce((sum, value) => sum + value, 0) / values.length;
}

function toIsoNow(): string {
  return new Date().toISOString();
}

function topBottomFromMomentum(entries: RankedPlayer[]): { risers: TrendPlayer[]; fallers: TrendPlayer[] } {
  const risers = [...entries]
    .sort((a, b) => b.momentum - a.momentum)
    .slice(0, 3)
    .map(({ momentum: _momentum, ...player }) => player);
  const riserIds = new Set(risers.map((player) => player.id));
  const fallers = [...entries]
    .sort((a, b) => a.momentum - b.momentum)
    .filter((player) => !riserIds.has(player.id))
    .slice(0, 3)
    .map(({ momentum: _momentum, ...player }) => player);

  return { risers, fallers };
}

function fallbackFromCurrentScores(localDate: string): WeeklyTrends {
  const hitters = topBottomFromMomentum(
    getCurrentScores("hitter").map((player) => ({ ...player, momentum: player.score }))
  );
  const pitchers = topBottomFromMomentum(
    getCurrentScores("pitcher").map((player) => ({ ...player, momentum: player.score }))
  );

  return {
    generatedAt: toIsoNow(),
    weekStart: getWeekStartFromDateString(localDate),
    hitters,
    pitchers,
  };
}

async function getPool(): Promise<Pool | null> {
  if (!poolPromise) {
    poolPromise = (async () => {
      const databaseUrl = process.env.DATABASE_URL?.trim();
      if (!databaseUrl) {
        return null;
      }

      const pool = new Pool({
        connectionString: databaseUrl,
        ssl:
          databaseUrl.includes("render.com") || process.env.NODE_ENV === "production"
            ? { rejectUnauthorized: false }
            : undefined,
      });

      await pool.query("SELECT 1");
      return pool;
    })();
  }

  return poolPromise;
}

async function initializeTables(pool: Pool): Promise<void> {
  if (initialized) {
    return;
  }

  await pool.query(`
    CREATE TABLE IF NOT EXISTS daily_trend_snapshots (
      snapshot_date DATE NOT NULL,
      player_type TEXT NOT NULL,
      player_id TEXT NOT NULL,
      player_name TEXT NOT NULL,
      mlbamid TEXT,
      score DOUBLE PRECISION NOT NULL,
      created_at TIMESTAMPTZ DEFAULT NOW(),
      PRIMARY KEY (snapshot_date, player_type, player_id)
    )
  `);

  await pool.query(`
    CREATE TABLE IF NOT EXISTS daily_trend_rankings (
      snapshot_date DATE NOT NULL,
      player_type TEXT NOT NULL,
      bucket TEXT NOT NULL,
      rank INT NOT NULL,
      player_id TEXT NOT NULL,
      player_name TEXT NOT NULL,
      mlbamid TEXT,
      momentum DOUBLE PRECISION NOT NULL,
      created_at TIMESTAMPTZ DEFAULT NOW(),
      PRIMARY KEY (snapshot_date, player_type, bucket, rank)
    )
  `);

  await pool.query(`
    CREATE INDEX IF NOT EXISTS idx_daily_trend_snapshots_date
    ON daily_trend_snapshots (snapshot_date, player_type)
  `);

  initialized = true;
}

async function ensureSnapshotForDate(pool: Pool, snapshotDate: string): Promise<void> {
  const existing = await pool.query(
    `SELECT COUNT(*)::int AS count FROM daily_trend_snapshots WHERE snapshot_date = $1`,
    [snapshotDate]
  );
  const count = Number((existing.rows[0] as { count?: number } | undefined)?.count ?? 0);

  if (count > 0) {
    return;
  }

  const hitters = getCurrentScores("hitter");
  const pitchers = getCurrentScores("pitcher");
  const allScores = [...hitters, ...pitchers];

  if (allScores.length === 0) {
    return;
  }

  const valuesSql: string[] = [];
  const params: Array<string | number | null> = [];
  allScores.forEach((player, index) => {
    const base = index * 6;
    valuesSql.push(`($${base + 1}, $${base + 2}, $${base + 3}, $${base + 4}, $${base + 5}, $${base + 6})`);
    params.push(
      snapshotDate,
      player.type,
      player.id,
      player.name,
      player.mlbamid ?? null,
      player.score
    );
  });

  await pool.query(
    `
      INSERT INTO daily_trend_snapshots
      (snapshot_date, player_type, player_id, player_name, mlbamid, score)
      VALUES ${valuesSql.join(", ")}
      ON CONFLICT (snapshot_date, player_type, player_id) DO NOTHING
    `,
    params
  );
}

async function ensureRankingsForDate(pool: Pool, snapshotDate: string): Promise<void> {
  const existing = await pool.query(
    `SELECT COUNT(*)::int AS count FROM daily_trend_rankings WHERE snapshot_date = $1`,
    [snapshotDate]
  );
  const count = Number((existing.rows[0] as { count?: number } | undefined)?.count ?? 0);
  if (count > 0) {
    return;
  }

  const recentStart = shiftDate(snapshotDate, -6);
  const priorStart = shiftDate(snapshotDate, -13);
  const priorEnd = shiftDate(snapshotDate, -7);

  const snapshotRows = await pool.query(
    `
      SELECT snapshot_date::text AS snapshot_date, player_type, player_id, player_name, mlbamid, score
      FROM daily_trend_snapshots
      WHERE snapshot_date >= $1 AND snapshot_date <= $2
    `,
    [priorStart, snapshotDate]
  );

  const byType = {
    hitter: new Map<string, { name: string; mlbamid: string | null; series: Array<{ date: string; score: number }> }>(),
    pitcher: new Map<string, { name: string; mlbamid: string | null; series: Array<{ date: string; score: number }> }>(),
  };

  (snapshotRows.rows as Array<{
    snapshot_date: string;
    player_type: "hitter" | "pitcher";
    player_id: string;
    player_name: string;
    mlbamid: string | null;
    score: number;
  }>).forEach((row) => {
    const type = row.player_type;
    const id = row.player_id;
    const existingEntry = byType[type].get(id) ?? {
      name: row.player_name,
      mlbamid: row.mlbamid,
      series: [],
    };
    existingEntry.series.push({ date: row.snapshot_date, score: Number(row.score) });
    byType[type].set(id, existingEntry);
  });

  const rowsToInsert: Array<{
    playerType: "hitter" | "pitcher";
    bucket: "risers" | "fallers";
    rank: number;
    playerId: string;
    playerName: string;
    mlbamid: string | null;
    momentum: number;
  }> = [];

  (["hitter", "pitcher"] as const).forEach((playerType) => {
    const ranked: RankedPlayer[] = [];
    byType[playerType].forEach((entry, playerId) => {
      const recentValues = entry.series
        .filter((point) => inRange(point.date, recentStart, snapshotDate))
        .map((point) => point.score);
      const priorValues = entry.series
        .filter((point) => inRange(point.date, priorStart, priorEnd))
        .map((point) => point.score);

      const recentAvg = avg(recentValues);
      const priorAvg = avg(priorValues);
      if (recentAvg === null || priorAvg === null) {
        return;
      }
      if (recentValues.length < 3 || priorValues.length < 3) {
        return;
      }

      ranked.push({
        id: playerId,
        name: entry.name,
        type: playerType,
        mlbamid: entry.mlbamid,
        momentum: recentAvg - priorAvg,
      });
    });

    const sortedRisers = [...ranked].sort((a, b) => b.momentum - a.momentum).slice(0, 3);
    const riserIds = new Set(sortedRisers.map((player) => player.id));
    const sortedFallers = [...ranked]
      .sort((a, b) => a.momentum - b.momentum)
      .filter((player) => !riserIds.has(player.id))
      .slice(0, 3);

    sortedRisers.forEach((player, index) => {
      rowsToInsert.push({
        playerType,
        bucket: "risers",
        rank: index + 1,
        playerId: player.id,
        playerName: player.name,
        mlbamid: player.mlbamid ?? null,
        momentum: player.momentum,
      });
    });

    sortedFallers.forEach((player, index) => {
      rowsToInsert.push({
        playerType,
        bucket: "fallers",
        rank: index + 1,
        playerId: player.id,
        playerName: player.name,
        mlbamid: player.mlbamid ?? null,
        momentum: player.momentum,
      });
    });
  });

  if (rowsToInsert.length === 0) {
    return;
  }

  const valuesSql: string[] = [];
  const params: Array<string | number | null> = [];
  rowsToInsert.forEach((row, index) => {
    const base = index * 8;
    valuesSql.push(
      `($${base + 1}, $${base + 2}, $${base + 3}, $${base + 4}, $${base + 5}, $${base + 6}, $${base + 7}, $${base + 8})`
    );
    params.push(
      snapshotDate,
      row.playerType,
      row.bucket,
      row.rank,
      row.playerId,
      row.playerName,
      row.mlbamid,
      row.momentum
    );
  });

  await pool.query(
    `
      INSERT INTO daily_trend_rankings
      (snapshot_date, player_type, bucket, rank, player_id, player_name, mlbamid, momentum)
      VALUES ${valuesSql.join(", ")}
      ON CONFLICT (snapshot_date, player_type, bucket, rank) DO UPDATE
      SET player_id = EXCLUDED.player_id,
          player_name = EXCLUDED.player_name,
          mlbamid = EXCLUDED.mlbamid,
          momentum = EXCLUDED.momentum
    `,
    params
  );
}

type RankingRow = {
  player_type: "hitter" | "pitcher";
  bucket: "risers" | "fallers";
  rank: number;
  player_id: string;
  player_name: string;
  mlbamid: string | null;
};

function bucketRowsToPlayers(rows: RankingRow[], playerType: "hitter" | "pitcher", bucket: "risers" | "fallers"): TrendPlayer[] {
  return rows
    .filter((row) => row.player_type === playerType && row.bucket === bucket)
    .sort((a, b) => a.rank - b.rank)
    .map((row) => ({
      id: row.player_id,
      name: row.player_name,
      type: playerType,
      mlbamid: row.mlbamid,
    }));
}

export async function getWeeklyTrends(): Promise<WeeklyTrends> {
  const localDate = getLocalDate(new Date(), DEFAULT_TIMEZONE);
  const weekStart = getWeekStartFromDateString(localDate);
  const pool = await getPool();

  if (!pool) {
    return fallbackFromCurrentScores(localDate);
  }

  await initializeTables(pool);
  await ensureSnapshotForDate(pool, localDate);
  await ensureRankingsForDate(pool, localDate);

  const rankingRows = await pool.query(
    `
      SELECT player_type, bucket, rank, player_id, player_name, mlbamid
      FROM daily_trend_rankings
      WHERE snapshot_date = $1
      ORDER BY player_type, bucket, rank
    `,
    [localDate]
  );

  if (rankingRows.rows.length === 0) {
    return fallbackFromCurrentScores(localDate);
  }

  const typedRows = rankingRows.rows as RankingRow[];

  return {
    generatedAt: toIsoNow(),
    weekStart,
    hitters: {
      risers: bucketRowsToPlayers(typedRows, "hitter", "risers"),
      fallers: bucketRowsToPlayers(typedRows, "hitter", "fallers"),
    },
    pitchers: {
      risers: bucketRowsToPlayers(typedRows, "pitcher", "risers"),
      fallers: bucketRowsToPlayers(typedRows, "pitcher", "fallers"),
    },
  };
}

