import { Pool } from "pg";

let poolPromise: Promise<Pool | null> | null = null;

export function getPool(): Promise<Pool | null> {
  if (!poolPromise) {
    const attempt = (async () => {
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
        max: 5,
        idleTimeoutMillis: 30_000,
        connectionTimeoutMillis: 5_000,
      });
      // An error on an idle client would otherwise be an unhandled "error" event and crash the process.
      pool.on("error", (err) => console.warn("[db] idle client error", err));

      try {
        await pool.query("SELECT 1");
      } catch (error) {
        await pool.end().catch(() => undefined);
        throw error;
      }
      return pool;
    })();
    poolPromise = attempt;
    // Don't cache a failed connection: the next call retries instead of disabling the database until restart.
    attempt.catch(() => {
      if (poolPromise === attempt) {
        poolPromise = null;
      }
    });
  }

  return poolPromise;
}
