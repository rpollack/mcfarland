import path from "node:path";
import sqlite3 from "sqlite3";
import { Pool } from "pg";

const SQLITE_DB_NAME = "analytics.db";
const SQLITE_FALLBACK_DB_NAME = "analytics_fallback.db";

type DatabaseDriver =
  | { type: "postgres"; pool: Pool }
  | { type: "sqlite"; db: sqlite3.Database };

let driverPromise: Promise<DatabaseDriver> | null = null;
let initializationPromise: Promise<void> | null = null;

function getAdminPassword(): string | undefined {
  const password = process.env.ADMIN_PASSWORD;
  if (!password || password.trim().length === 0) {
    return undefined;
  }
  return password;
}

function containsAdminSecret(value: string | null | undefined): boolean {
  const adminPassword = getAdminPassword();
  if (!adminPassword) {
    return false;
  }
  if (!value) {
    return false;
  }

  const encoded = encodeURIComponent(adminPassword);
  if (value.includes(`admin=${adminPassword}`) || value.includes(`admin=${encoded}`)) {
    return true;
  }

  try {
    const decoded = decodeURIComponent(value);
    return decoded.includes(`admin=${adminPassword}`);
  } catch (_error) {
    return false;
  }
}

function shouldSkipLogging(...sources: Array<string | null | undefined>): boolean {
  return sources.some((source) => containsAdminSecret(source));
}

async function createSqliteDriver(): Promise<DatabaseDriver> {
  const isRender = process.env.RENDER === "true";
  const dbName = isRender ? SQLITE_FALLBACK_DB_NAME : SQLITE_DB_NAME;
  const dbPath = path.resolve(process.cwd(), dbName);
  const sqlite = sqlite3.verbose();

  const db = await new Promise<sqlite3.Database>((resolve, reject) => {
    const instance = new sqlite.Database(dbPath, (error) => {
      if (error) {
        reject(error);
      } else {
        resolve(instance);
      }
    });
  });

  console.info(`[analytics] using SQLite database at ${dbPath}`);
  return { type: "sqlite", db };
}

async function createPostgresDriver(databaseUrl: string): Promise<DatabaseDriver> {
  const pool = new Pool({
    connectionString: databaseUrl,
    ssl:
      databaseUrl.includes("render.com") || process.env.NODE_ENV === "production"
        ? { rejectUnauthorized: false }
        : undefined,
  });

  await pool.query("SELECT 1");
  console.info("[analytics] connected to PostgreSQL database");
  return { type: "postgres", pool };
}

async function getDriver(): Promise<DatabaseDriver> {
  if (!driverPromise) {
    driverPromise = (async () => {
      const databaseUrl = process.env.DATABASE_URL;
      if (databaseUrl && databaseUrl.trim().length > 0) {
        try {
          return await createPostgresDriver(databaseUrl);
        } catch (error) {
          console.warn("[analytics] failed to connect to PostgreSQL, falling back to SQLite", error);
        }
      }

      return createSqliteDriver();
    })();
  }

  return driverPromise;
}

function runSqlite(db: sqlite3.Database, sql: string, params: unknown[] = []): Promise<void> {
  return new Promise((resolve, reject) => {
    db.run(sql, params, function (error) {
      if (error) {
        reject(error);
      } else {
        resolve();
      }
    });
  });
}

async function runStatement(sqliteSql: string, postgresSql: string, params: unknown[] = []): Promise<void> {
  const driver = await getDriver();

  if (driver.type === "postgres") {
    await driver.pool.query(postgresSql, params);
  } else {
    await runSqlite(driver.db, sqliteSql, params);
  }
}

function truncate(value: string | null | undefined, maxLength: number): string | null {
  if (!value) {
    return null;
  }
  if (value.length <= maxLength) {
    return value;
  }
  return value.slice(0, maxLength);
}

export async function initializeAnalytics(): Promise<void> {
  if (!initializationPromise) {
    initializationPromise = (async () => {
      await runStatement(
        `CREATE TABLE IF NOT EXISTS sessions (
          session_id TEXT,
          referer TEXT,
          created_at DATETIME DEFAULT CURRENT_TIMESTAMP
        )`,
        `CREATE TABLE IF NOT EXISTS sessions (
          session_id TEXT,
          referer TEXT,
          created_at TIMESTAMPTZ DEFAULT NOW()
        )`
      );

      await runStatement(
        `CREATE TABLE IF NOT EXISTS analyses (
          session_id TEXT,
          player_label TEXT,
          analysis_mode TEXT,
          event_type TEXT,
          player_type TEXT,
          referer TEXT,
          created_at DATETIME DEFAULT CURRENT_TIMESTAMP
        )`,
        `CREATE TABLE IF NOT EXISTS analyses (
          session_id TEXT,
          player_label TEXT,
          analysis_mode TEXT,
          event_type TEXT,
          player_type TEXT,
          referer TEXT,
          created_at TIMESTAMPTZ DEFAULT NOW()
        )`
      );

      await runStatement(
        `CREATE TABLE IF NOT EXISTS share_events (
          session_id TEXT,
          player_label TEXT,
          analysis_mode TEXT,
          event_type TEXT,
          player_type TEXT,
          share_url TEXT,
          referer TEXT,
          created_at DATETIME DEFAULT CURRENT_TIMESTAMP
        )`,
        `CREATE TABLE IF NOT EXISTS share_events (
          session_id TEXT,
          player_label TEXT,
          analysis_mode TEXT,
          event_type TEXT,
          player_type TEXT,
          share_url TEXT,
          referer TEXT,
          created_at TIMESTAMPTZ DEFAULT NOW()
        )`
      );

      await runStatement(
        `CREATE INDEX IF NOT EXISTS idx_sessions_created_at ON sessions(date(created_at))`,
        `CREATE INDEX IF NOT EXISTS idx_sessions_created_at ON sessions((created_at::date))`
      );

      await runStatement(
        `CREATE INDEX IF NOT EXISTS idx_analyses_created_at ON analyses(date(created_at))`,
        `CREATE INDEX IF NOT EXISTS idx_analyses_created_at ON analyses((created_at::date))`
      );

      await runStatement(
        `CREATE INDEX IF NOT EXISTS idx_share_events_created_at ON share_events(date(created_at))`,
        `CREATE INDEX IF NOT EXISTS idx_share_events_created_at ON share_events((created_at::date))`
      );
    })().catch((error) => {
      console.warn("[analytics] failed to initialize analytics storage", error);
    });
  }

  try {
    await initializationPromise;
  } catch (_error) {
    // Initialization failure already logged; continue without blocking requests.
  }
}

type AnalysisLogEvent = {
  sessionId: string;
  playerLabel: string;
  analysisMode: string;
  playerType: "hitter" | "pitcher";
  eventType: "single" | "compare";
  referer?: string | null;
};

type ShareLogEvent = {
  sessionId: string;
  playerLabel: string;
  analysisMode: string;
  eventType: string;
  playerType?: "hitter" | "pitcher";
  shareUrl?: string | null;
  referer?: string | null;
};

export async function logSessionStart(sessionId: string, referer?: string | null): Promise<void> {
  if (!sessionId) {
    return;
  }
  if (shouldSkipLogging(referer)) {
    return;
  }

  try {
    await initializeAnalytics();
    await runStatement(
      `INSERT INTO sessions (session_id, referer) VALUES (?, ?)`,
      `INSERT INTO sessions (session_id, referer) VALUES ($1, $2)`,
      [sessionId, truncate(referer, 512)]
    );
    console.info("[analytics] session started", { sessionId });
  } catch (error) {
    console.warn("[analytics] failed to log session start", error);
  }
}

export async function logAnalysisEvent(event: AnalysisLogEvent): Promise<void> {
  if (!event.sessionId) {
    return;
  }
  if (shouldSkipLogging(event.referer)) {
    return;
  }

  try {
    await initializeAnalytics();
    await runStatement(
      `INSERT INTO analyses (session_id, player_label, analysis_mode, event_type, player_type, referer)
       VALUES (?, ?, ?, ?, ?, ?)`,
      `INSERT INTO analyses (session_id, player_label, analysis_mode, event_type, player_type, referer)
       VALUES ($1, $2, $3, $4, $5, $6)` ,
      [
        event.sessionId,
        truncate(event.playerLabel, 256),
        truncate(event.analysisMode, 64),
        event.eventType,
        event.playerType,
        truncate(event.referer, 512),
      ]
    );
    console.info("[analytics] analysis logged", {
      sessionId: event.sessionId,
      playerLabel: event.playerLabel,
      analysisMode: event.analysisMode,
      eventType: event.eventType,
    });
  } catch (error) {
    console.warn("[analytics] failed to log analysis", error);
  }
}

export async function logShareEvent(event: ShareLogEvent): Promise<void> {
  if (!event.sessionId) {
    return;
  }
  if (shouldSkipLogging(event.referer, event.shareUrl)) {
    return;
  }

  try {
    await initializeAnalytics();
    await runStatement(
      `INSERT INTO share_events (session_id, player_label, analysis_mode, event_type, player_type, share_url, referer)
       VALUES (?, ?, ?, ?, ?, ?, ?)`,
      `INSERT INTO share_events (session_id, player_label, analysis_mode, event_type, player_type, share_url, referer)
       VALUES ($1, $2, $3, $4, $5, $6, $7)` ,
      [
        event.sessionId,
        truncate(event.playerLabel, 256),
        truncate(event.analysisMode, 64),
        truncate(event.eventType, 64),
        event.playerType ?? null,
        truncate(event.shareUrl ?? null, 1024),
        truncate(event.referer, 512),
      ]
    );
    console.info("[analytics] share event logged", {
      sessionId: event.sessionId,
      eventType: event.eventType,
      playerLabel: event.playerLabel,
    });
  } catch (error) {
    console.warn("[analytics] failed to log share event", error);
  }
}
