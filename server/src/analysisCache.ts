import crypto from "node:crypto";
import { Pool } from "pg";
import type { PlayerType } from "./types.js";
import type { AnalysisMode } from "./vibes.js";

const ANALYSIS_CACHE_VERSION = "analysis-json-v1";

type CachedAnalysis = {
  headline: string;
  analysis: string;
};

type CacheLookup = {
  cacheKey: string;
  dataThroughDate: string;
};

type CacheWrite = CacheLookup & {
  analysisMode: AnalysisMode;
  playerType: PlayerType;
  playerIds: string[];
  headline: string;
  analysis: string;
};

type AnalysisCacheStore = {
  get: (lookup: CacheLookup) => Promise<CachedAnalysis | null>;
  set: (entry: CacheWrite) => Promise<void>;
  prune: (dataThroughDate: string) => Promise<void>;
};

let poolPromise: Promise<Pool | null> | null = null;
let initialized = false;
let testStore: AnalysisCacheStore | null = null;

export function buildAnalysisCacheKey(mode: AnalysisMode, prompt: string): string {
  return crypto
    .createHash("sha256")
    .update(`${ANALYSIS_CACHE_VERSION}:${mode}:${prompt}`)
    .digest("hex");
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

async function initializeTable(pool: Pool): Promise<void> {
  if (initialized) {
    return;
  }

  await pool.query(`
    CREATE TABLE IF NOT EXISTS analysis_cache (
      cache_key TEXT PRIMARY KEY,
      data_through_date DATE NOT NULL,
      analysis_mode TEXT NOT NULL,
      player_type TEXT NOT NULL,
      player_ids TEXT[] NOT NULL,
      headline TEXT NOT NULL,
      analysis TEXT NOT NULL,
      created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
      last_accessed_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
      hit_count INTEGER NOT NULL DEFAULT 0
    )
  `);

  await pool.query(`
    CREATE INDEX IF NOT EXISTS idx_analysis_cache_data_through
    ON analysis_cache (data_through_date)
  `);

  initialized = true;
}

async function getStore(): Promise<AnalysisCacheStore | null> {
  if (testStore) {
    return testStore;
  }

  const pool = await getPool();
  if (!pool) {
    return null;
  }
  await initializeTable(pool);

  return {
    async get({ cacheKey, dataThroughDate }) {
      const result = await pool.query(
        `
          UPDATE analysis_cache
          SET last_accessed_at = NOW(),
              hit_count = hit_count + 1
          WHERE cache_key = $1
            AND data_through_date = $2
          RETURNING headline, analysis
        `,
        [cacheKey, dataThroughDate]
      );

      const row = result.rows[0] as CachedAnalysis | undefined;
      return row ?? null;
    },
    async set(entry) {
      await pool.query(
        `
          INSERT INTO analysis_cache (
            cache_key,
            data_through_date,
            analysis_mode,
            player_type,
            player_ids,
            headline,
            analysis
          )
          VALUES ($1, $2, $3, $4, $5, $6, $7)
          ON CONFLICT (cache_key) DO UPDATE SET
            data_through_date = EXCLUDED.data_through_date,
            analysis_mode = EXCLUDED.analysis_mode,
            player_type = EXCLUDED.player_type,
            player_ids = EXCLUDED.player_ids,
            headline = EXCLUDED.headline,
            analysis = EXCLUDED.analysis,
            last_accessed_at = NOW()
        `,
        [
          entry.cacheKey,
          entry.dataThroughDate,
          entry.analysisMode,
          entry.playerType,
          entry.playerIds,
          entry.headline,
          entry.analysis,
        ]
      );
    },
    async prune(dataThroughDate) {
      await pool.query(
        `
          DELETE FROM analysis_cache
          WHERE data_through_date <> $1
        `,
        [dataThroughDate]
      );
    },
  };
}

export async function getCachedAnalysis(lookup: CacheLookup): Promise<CachedAnalysis | null> {
  try {
    const store = await getStore();
    return store ? await store.get(lookup) : null;
  } catch (error) {
    console.warn("[analysis-cache] read failed; bypassing cache", error);
    return null;
  }
}

export async function saveCachedAnalysis(entry: CacheWrite): Promise<void> {
  try {
    const store = await getStore();
    if (!store) {
      return;
    }
    await store.set(entry);
    await store.prune(entry.dataThroughDate);
  } catch (error) {
    console.warn("[analysis-cache] write failed; continuing without cache", error);
  }
}

export function __setAnalysisCacheStoreForTests(store: AnalysisCacheStore | null): void {
  testStore = store;
}
