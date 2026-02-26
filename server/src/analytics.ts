import { Pool } from "pg";
import { containsAdminSecret } from "./admin.js";

type DatabaseDriver = { type: "postgres"; pool: Pool } | { type: "simulated" };

let driverPromise: Promise<DatabaseDriver> | null = null;
let initializationPromise: Promise<void> | null = null;
const DEFAULT_AMPLITUDE_API_KEY = "f7a70fb74292a0b7b9f5dced71f2931";
const AMPLITUDE_HTTP_API = "https://api2.amplitude.com/2/httpapi";

function shouldSkipLogging(...sources: Array<string | null | undefined>): boolean {
  return sources.some((source) => containsAdminSecret(source));
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
          console.warn("[analytics] failed to connect to PostgreSQL, using simulated logging", error);
        }
      } else if (isProduction) {
        throw new Error("DATABASE_URL must be configured in production");
      }

      console.info("[analytics] using simulated logging (no DATABASE_URL configured)");
      return { type: "simulated" };
    })();
  }

  return driverPromise;
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

type AmplitudeEvent = {
  event_type: string;
  user_id: string;
  time?: number;
  event_properties?: Record<string, unknown>;
};

async function sendAmplitudeEvent(event: AmplitudeEvent): Promise<void> {
  const apiKey = (process.env.AMPLITUDE_API_KEY ?? DEFAULT_AMPLITUDE_API_KEY).trim();
  if (!apiKey) {
    return;
  }

  try {
    const response = await fetch(AMPLITUDE_HTTP_API, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        api_key: apiKey,
        events: [event],
      }),
    });

    if (!response.ok) {
      const body = await response.text().catch(() => "");
      console.warn("[analytics] failed to forward event to Amplitude", {
        eventType: event.event_type,
        status: response.status,
        body: body.slice(0, 500),
      });
    }
  } catch (error) {
    console.warn("[analytics] failed to reach Amplitude", {
      eventType: event.event_type,
      error,
    });
  }
}

export async function initializeAnalytics(): Promise<void> {
  const driver = await getDriver();
  if (driver.type !== "postgres") {
    return;
  }

  if (!initializationPromise) {
    const pool = driver.pool;
    initializationPromise = (async () => {
      await pool.query(`CREATE TABLE IF NOT EXISTS sessions (
          user_id TEXT PRIMARY KEY,
          timestamp TIMESTAMPTZ DEFAULT NOW()
        )`);

      await pool.query(`CREATE TABLE IF NOT EXISTS analyses (
          user_id TEXT,
          player_name TEXT,
          analysis_mode TEXT,
          timestamp TIMESTAMPTZ DEFAULT NOW()
        )`);

      await pool.query(`CREATE TABLE IF NOT EXISTS share_events (
          user_id TEXT,
          player_name TEXT,
          analysis_mode TEXT,
          event_type TEXT,
          player_type TEXT,
          share_url TEXT,
          referer TEXT,
          timestamp TIMESTAMPTZ DEFAULT NOW()
        )`);

      await pool.query(
        `CREATE INDEX IF NOT EXISTS idx_sessions_timestamp ON sessions((timestamp::date))`
      );
      await pool.query(
        `CREATE INDEX IF NOT EXISTS idx_analyses_timestamp ON analyses((timestamp::date))`
      );
      await pool.query(
        `CREATE INDEX IF NOT EXISTS idx_share_events_timestamp ON share_events((timestamp::date))`
      );
    })();
  }

  try {
    await initializationPromise;
  } catch (error) {
    console.warn("[analytics] failed to initialize analytics storage", error);
  }
}

function simulateLogging(
  table: "sessions" | "analyses" | "share_events",
  columns: string[],
  values: Record<string, unknown>
): void {
  console.info(
    `Simulated logging: database=PostgreSQL table=${table} columns=[${columns.join(", ")}] values=${JSON.stringify(
      values
    )}`
  );
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

  const driver = await getDriver();

  if (driver.type !== "postgres") {
    simulateLogging("sessions", ["user_id", "timestamp"], {
      user_id: sessionId,
      timestamp: new Date().toISOString(),
    });
    await sendAmplitudeEvent({
      event_type: "session_started",
      user_id: sessionId,
      time: Date.now(),
      event_properties: {
        table: "sessions",
        referer: truncate(referer, 512),
      },
    });
    return;
  }

  try {
    await initializeAnalytics();
    const result = await driver.pool.query(
      `INSERT INTO sessions (user_id)
       SELECT $1
       WHERE NOT EXISTS (
         SELECT 1 FROM sessions WHERE user_id = $1
       )`,
      [sessionId]
    );
    if (result.rowCount && result.rowCount > 0) {
      await sendAmplitudeEvent({
        event_type: "session_started",
        user_id: sessionId,
        time: Date.now(),
        event_properties: {
          table: "sessions",
          referer: truncate(referer, 512),
        },
      });
    }
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
  const driver = await getDriver();
  const insertValues = {
    user_id: event.sessionId,
    player_name: truncate(event.playerName, 256),
    analysis_mode: truncate(event.analysisMode, 64),
  };
  const timestamp = new Date().toISOString();

  if (driver.type !== "postgres") {
    simulateLogging("analyses", ["user_id", "player_name", "analysis_mode", "timestamp"], {
      ...insertValues,
      timestamp,
    });
    await sendAmplitudeEvent({
      event_type: "analysis_logged",
      user_id: event.sessionId,
      time: Date.now(),
      event_properties: {
        table: "analyses",
        player_name: insertValues.player_name,
        analysis_mode: insertValues.analysis_mode,
        player_type: event.playerType,
        event_type: event.eventType,
        referer: truncate(event.referer, 512),
      },
    });
    return;
  }

  try {
    await initializeAnalytics();
    await driver.pool.query(`INSERT INTO analyses (user_id, player_name, analysis_mode) VALUES ($1, $2, $3)`, [
      insertValues.user_id,
      insertValues.player_name,
      insertValues.analysis_mode,
    ]);
    await sendAmplitudeEvent({
      event_type: "analysis_logged",
      user_id: event.sessionId,
      time: Date.now(),
      event_properties: {
        table: "analyses",
        player_name: insertValues.player_name,
        analysis_mode: insertValues.analysis_mode,
        player_type: event.playerType,
        event_type: event.eventType,
        referer: truncate(event.referer, 512),
      },
    });
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

  const driver = await getDriver();
  const values = {
    user_id: event.sessionId,
    player_name: truncate(event.playerName, 256),
    analysis_mode: truncate(event.analysisMode, 64),
    event_type: truncate(event.eventType, 64),
    player_type: event.playerType ?? null,
    share_url: truncate(event.shareUrl ?? null, 1024),
    referer: truncate(event.referer, 512),
    timestamp: new Date().toISOString(),
  };

  if (driver.type !== "postgres") {
    simulateLogging("share_events", Object.keys(values), values);
    await sendAmplitudeEvent({
      event_type: "share_event_logged",
      user_id: event.sessionId,
      time: Date.now(),
      event_properties: {
        table: "share_events",
        player_name: values.player_name,
        analysis_mode: values.analysis_mode,
        event_type: values.event_type,
        player_type: values.player_type,
        share_url: values.share_url,
        referer: values.referer,
      },
    });
    return;
  }

  try {
    await initializeAnalytics();
    await driver.pool.query(
      `INSERT INTO share_events (user_id, player_name, analysis_mode, event_type, player_type, share_url, referer)
       VALUES ($1, $2, $3, $4, $5, $6, $7)`,
      [
        values.user_id,
        values.player_name,
        values.analysis_mode,
        values.event_type,
        values.player_type,
        values.share_url,
        values.referer,
      ]
    );
    await sendAmplitudeEvent({
      event_type: "share_event_logged",
      user_id: event.sessionId,
      time: Date.now(),
      event_properties: {
        table: "share_events",
        player_name: values.player_name,
        analysis_mode: values.analysis_mode,
        event_type: values.event_type,
        player_type: values.player_type,
        share_url: values.share_url,
        referer: values.referer,
      },
    });
    console.info("[analytics] share event logged", {
      sessionId: event.sessionId,
      eventType: event.eventType,
      playerName: event.playerName,
    });
  } catch (error) {
    console.warn("[analytics] failed to log share event", error);
  }
}
