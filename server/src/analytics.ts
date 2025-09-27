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
  sqlite3.verbose();

  const db = await new Promise<sqlite3.Database>((resolve, reject) => {
    try {
      const instance = new sqlite3.Database(dbPath);
      instance.get("SELECT 1", (error) => {
        if (error) {
          reject(error);
        } else {
          resolve(instance);
        }
      });
    } catch (error) {
      reject(error as Error);
    }
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
      const databaseUrl = process.env.DATABASE_URL?.trim();
      const isProduction = process.env.NODE_ENV === "production";

      if (databaseUrl) {
        try {
          return await createPostgresDriver(databaseUrl);
        } catch (error) {
          if (isProduction) {
            console.error("[analytics] unable to connect to PostgreSQL in production", error);
            throw error;
          }
          console.warn("[analytics] failed to connect to PostgreSQL, falling back to SQLite", error);
        }
      } else if (isProduction) {
        throw new Error("DATABASE_URL must be configured in production");
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
          user_id TEXT PRIMARY KEY,
          referer TEXT,
          timestamp DATETIME DEFAULT CURRENT_TIMESTAMP
        )`,
        `CREATE TABLE IF NOT EXISTS sessions (
          user_id TEXT PRIMARY KEY,
          referer TEXT,
          timestamp TIMESTAMPTZ DEFAULT NOW()
        )`
      );

      await runStatement(
        `CREATE TABLE IF NOT EXISTS analyses (
          user_id TEXT,
          player_name TEXT,
          analysis_mode TEXT,
          event_type TEXT,
          player_type TEXT,
          referer TEXT,
          timestamp DATETIME DEFAULT CURRENT_TIMESTAMP
        )`,
        `CREATE TABLE IF NOT EXISTS analyses (
          user_id TEXT,
          player_name TEXT,
          analysis_mode TEXT,
          event_type TEXT,
          player_type TEXT,
          referer TEXT,
          timestamp TIMESTAMPTZ DEFAULT NOW()
        )`
      );

      await runStatement(
        `CREATE TABLE IF NOT EXISTS share_events (
          user_id TEXT,
          player_name TEXT,
          analysis_mode TEXT,
          event_type TEXT,
          player_type TEXT,
          share_url TEXT,
          referer TEXT,
          timestamp DATETIME DEFAULT CURRENT_TIMESTAMP
        )`,
        `CREATE TABLE IF NOT EXISTS share_events (
          user_id TEXT,
          player_name TEXT,
          analysis_mode TEXT,
          event_type TEXT,
          player_type TEXT,
          share_url TEXT,
          referer TEXT,
          timestamp TIMESTAMPTZ DEFAULT NOW()
        )`
      );

      await runStatement(
        `CREATE INDEX IF NOT EXISTS idx_sessions_timestamp ON sessions(date(timestamp))`,
        `CREATE INDEX IF NOT EXISTS idx_sessions_timestamp ON sessions((timestamp::date))`
      );

      await runStatement(
        `CREATE INDEX IF NOT EXISTS idx_analyses_timestamp ON analyses(date(timestamp))`,
        `CREATE INDEX IF NOT EXISTS idx_analyses_timestamp ON analyses((timestamp::date))`
      );

      await runStatement(
        `CREATE INDEX IF NOT EXISTS idx_share_events_timestamp ON share_events(date(timestamp))`,
        `CREATE INDEX IF NOT EXISTS idx_share_events_timestamp ON share_events((timestamp::date))`
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
  playerName: string;
  analysisMode: string;
  playerType: "hitter" | "pitcher";
  eventType: "single" | "compare";
  referer?: string | null;
};

type ShareLogEvent = {
  sessionId: string;
  playerName: string;
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
      `INSERT OR IGNORE INTO sessions (user_id, referer) VALUES (?, ?)`,
      `INSERT INTO sessions (user_id, referer) VALUES ($1, $2) ON CONFLICT (user_id) DO NOTHING`,
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
      `INSERT INTO analyses (user_id, player_name, analysis_mode, event_type, player_type, referer)
       VALUES (?, ?, ?, ?, ?, ?)`,
      `INSERT INTO analyses (user_id, player_name, analysis_mode, event_type, player_type, referer)
       VALUES ($1, $2, $3, $4, $5, $6)` ,
      [
        event.sessionId,
        truncate(event.playerName, 256),
        truncate(event.analysisMode, 64),
        event.eventType,
        event.playerType,
        truncate(event.referer, 512),
      ]
    );
    console.info("[analytics] analysis logged", {
      sessionId: event.sessionId,
      playerName: event.playerName,
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
      `INSERT INTO share_events (user_id, player_name, analysis_mode, event_type, player_type, share_url, referer)
       VALUES (?, ?, ?, ?, ?, ?, ?)`,
      `INSERT INTO share_events (user_id, player_name, analysis_mode, event_type, player_type, share_url, referer)
       VALUES ($1, $2, $3, $4, $5, $6, $7)` ,
      [
        event.sessionId,
        truncate(event.playerName, 256),
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
      playerName: event.playerName,
    });
  } catch (error) {
    console.warn("[analytics] failed to log share event", error);
  }
}
