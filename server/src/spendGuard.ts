import type { Response } from "express";
import type { Pool } from "pg";
import { logSpendDailySummary, logSpendLimitHit } from "./analytics.js";
import { getPool } from "./db.js";
import type { ModelCallKind, ModelUsageRecord } from "./modelUsage.js";
import { estimateCostUsd } from "./pricing.js";

/**
 * Model spend guard: a rolling dollar budget (month total + daily ceiling) and a per-IP hourly cap on cache-miss generations.
 *
 * - Cache hits never reach this module. Admin requests are never blocked, but their spend still counts.
 * - Cost is only known after a call, so checkBudget() runs before a call and recordSpend() after it. Calls already in
 *   flight can overshoot a limit by a few cents each; the per-IP cap and the route rate limiter bound how many.
 * - Fails open: if storage is unavailable the guard falls back to an in-process tally rather than blocking users.
 * - The per-IP window is process-local. That is correct for the single Render instance this app runs on.
 */

export type LimitType = "daily" | "monthly" | "ip";

const TOTALS_TTL_MS = 15_000;
const IP_WINDOW_MS = 60 * 60 * 1000;
const MAX_TRACKED_IPS = 10_000;
const WARN_INTERVAL_MS = 60 * 60 * 1000;
const DEFAULT_IP_HOURLY_CAP = 20;

type Agg = { costUsd: number; calls: number };
type DayHits = { day: string; daily: number; monthly: number; ip: number; distinctSessions: number };
type HitRow = {
  hitAt: Date;
  kind: ModelCallKind;
  limitType: LimitType;
  enforced: boolean;
  daySpendUsd: number;
  monthSpendUsd: number;
  sessionId: string | null;
};
type DayStats = { costUsd: number; calls: number; hits: number; distinctSessions: number };

export interface SpendStore {
  addSpend(day: string, kind: string, costUsd: number): Promise<void>;
  totals(day: string, monthStart: string, nextMonthStart: string): Promise<{ day: Agg; month: Agg }>;
  addHit(hit: HitRow): Promise<void>;
  hitsSince(sinceDay: string): Promise<DayHits[]>;
  dayStats(day: string): Promise<DayStats>;
}

export class SpendLimitError extends Error {
  readonly code = "spend_limit";
  constructor(
    readonly limitType: LimitType,
    readonly retryAfterSeconds: number,
    message: string
  ) {
    super(message);
    this.name = "SpendLimitError";
  }
}

type Config = { monthlyBudgetUsd: number | null; dailyCeilingUsd: number | null; ipHourlyCap: number; enforce: boolean };

function positiveNumber(raw: string | undefined): number | null {
  const value = Number(raw?.trim());
  return raw?.trim() && Number.isFinite(value) && value > 0 ? value : null;
}

export function readSpendConfig(): Config {
  const monthlyBudgetUsd = positiveNumber(process.env.MODEL_MONTHLY_BUDGET_USD);
  const dailyCeilingUsd = monthlyBudgetUsd === null ? null : positiveNumber(process.env.MODEL_DAILY_CEILING_USD) ?? monthlyBudgetUsd * 0.1;
  const rawIp = process.env.MODEL_IP_HOURLY_CAP?.trim();
  const parsedIp = rawIp ? Math.floor(Number(rawIp)) : NaN;
  const ipHourlyCap = Number.isFinite(parsedIp) && parsedIp >= 0 ? parsedIp : DEFAULT_IP_HOURLY_CAP;
  return { monthlyBudgetUsd, dailyCeilingUsd, ipHourlyCap, enforce: process.env.MODEL_CAP_ENFORCE?.trim() === "true" };
}

function dayKey(date: Date): string {
  return date.toISOString().slice(0, 10);
}
function monthStartKey(date: Date): string {
  return `${dayKey(date).slice(0, 7)}-01`;
}
function nextMonthStart(date: Date): Date {
  return new Date(Date.UTC(date.getUTCFullYear(), date.getUTCMonth() + 1, 1));
}
function nextDayStart(date: Date): Date {
  return new Date(Date.UTC(date.getUTCFullYear(), date.getUTCMonth(), date.getUTCDate() + 1));
}

// ---------------------------------------------------------------------------------------------------------------
// Stores

/** In-process store: the fail-open fallback, and the test double. */
export class MemorySpendStore implements SpendStore {
  private spend = new Map<string, Agg>(); // key: `${day}|${kind}`
  private hits: HitRow[] = [];

  async addSpend(day: string, kind: string, costUsd: number): Promise<void> {
    const key = `${day}|${kind}`;
    const current = this.spend.get(key) ?? { costUsd: 0, calls: 0 };
    this.spend.set(key, { costUsd: current.costUsd + costUsd, calls: current.calls + 1 });
  }

  async totals(day: string, monthStart: string, next: string): Promise<{ day: Agg; month: Agg }> {
    const result = { day: { costUsd: 0, calls: 0 }, month: { costUsd: 0, calls: 0 } };
    for (const [key, agg] of this.spend) {
      const entryDay = key.split("|")[0];
      if (entryDay === day) {
        result.day.costUsd += agg.costUsd;
        result.day.calls += agg.calls;
      }
      if (entryDay >= monthStart && entryDay < next) {
        result.month.costUsd += agg.costUsd;
        result.month.calls += agg.calls;
      }
    }
    return result;
  }

  async addHit(hit: HitRow): Promise<void> {
    this.hits.push(hit);
    if (this.hits.length > 5000) {
      this.hits.splice(0, this.hits.length - 5000);
    }
  }

  async hitsSince(sinceDay: string): Promise<DayHits[]> {
    const byDay = new Map<string, { row: DayHits; sessions: Set<string> }>();
    for (const hit of this.hits) {
      const day = dayKey(hit.hitAt);
      if (day < sinceDay) {
        continue;
      }
      const entry = byDay.get(day) ?? { row: { day, daily: 0, monthly: 0, ip: 0, distinctSessions: 0 }, sessions: new Set<string>() };
      entry.row[hit.limitType] += 1;
      if (hit.sessionId) {
        entry.sessions.add(hit.sessionId);
      }
      entry.row.distinctSessions = entry.sessions.size;
      byDay.set(day, entry);
    }
    return [...byDay.values()].map((entry) => entry.row).sort((a, b) => (a.day < b.day ? 1 : -1));
  }

  async dayStats(day: string): Promise<DayStats> {
    const totals = await this.totals(day, day, dayKey(new Date(Date.parse(`${day}T00:00:00Z`) + 86_400_000)));
    const sessions = new Set<string>();
    let hits = 0;
    for (const hit of this.hits) {
      if (dayKey(hit.hitAt) === day) {
        hits += 1;
        if (hit.sessionId) {
          sessions.add(hit.sessionId);
        }
      }
    }
    return { costUsd: totals.day.costUsd, calls: totals.day.calls, hits, distinctSessions: sessions.size };
  }
}

type Row = Record<string, string | undefined>;

let tablesReady = false;

async function ensureTables(pool: Pool): Promise<void> {
  if (tablesReady) {
    return;
  }
  await pool.query(`
    CREATE TABLE IF NOT EXISTS model_spend_daily (
      day DATE NOT NULL,
      kind TEXT NOT NULL,
      calls INTEGER NOT NULL DEFAULT 0,
      cost_usd NUMERIC(12,6) NOT NULL DEFAULT 0,
      PRIMARY KEY (day, kind)
    )
  `);
  await pool.query(`
    CREATE TABLE IF NOT EXISTS spend_limit_hits (
      id BIGSERIAL PRIMARY KEY,
      hit_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
      kind TEXT NOT NULL,
      limit_type TEXT NOT NULL,
      enforced BOOLEAN NOT NULL,
      day_spend_usd NUMERIC NOT NULL,
      month_spend_usd NUMERIC NOT NULL,
      session_id TEXT NULL
    )
  `);
  await pool.query(`CREATE INDEX IF NOT EXISTS idx_spend_limit_hits_hit_at ON spend_limit_hits (hit_at)`);
  tablesReady = true;
}

function createPostgresStore(pool: Pool): SpendStore {
  return {
    async addSpend(day, kind, costUsd) {
      await pool.query(
        `
          INSERT INTO model_spend_daily (day, kind, calls, cost_usd)
          VALUES ($1, $2, 1, $3)
          ON CONFLICT (day, kind) DO UPDATE SET
            calls = model_spend_daily.calls + 1,
            cost_usd = model_spend_daily.cost_usd + EXCLUDED.cost_usd
        `,
        [day, kind, costUsd]
      );
    },
    async totals(day, monthStart, next) {
      const result = await pool.query(
        `
          SELECT
            COALESCE(SUM(cost_usd) FILTER (WHERE day = $1), 0) AS day_cost,
            COALESCE(SUM(calls) FILTER (WHERE day = $1), 0) AS day_calls,
            COALESCE(SUM(cost_usd), 0) AS month_cost,
            COALESCE(SUM(calls), 0) AS month_calls
          FROM model_spend_daily
          WHERE day >= $2 AND day < $3
        `,
        [day, monthStart, next]
      );
      const row = (result.rows as Row[])[0] ?? ({} as Row);
      return {
        day: { costUsd: Number(row.day_cost ?? 0), calls: Number(row.day_calls ?? 0) },
        month: { costUsd: Number(row.month_cost ?? 0), calls: Number(row.month_calls ?? 0) },
      };
    },
    async addHit(hit) {
      await pool.query(
        `
          INSERT INTO spend_limit_hits (hit_at, kind, limit_type, enforced, day_spend_usd, month_spend_usd, session_id)
          VALUES ($1, $2, $3, $4, $5, $6, $7)
        `,
        [hit.hitAt, hit.kind, hit.limitType, hit.enforced, hit.daySpendUsd, hit.monthSpendUsd, hit.sessionId]
      );
    },
    async hitsSince(sinceDay) {
      const result = await pool.query(
        `
          SELECT
            to_char(hit_at AT TIME ZONE 'UTC', 'YYYY-MM-DD') AS day,
            COUNT(*) FILTER (WHERE limit_type = 'daily') AS daily,
            COUNT(*) FILTER (WHERE limit_type = 'monthly') AS monthly,
            COUNT(*) FILTER (WHERE limit_type = 'ip') AS ip,
            COUNT(DISTINCT session_id) AS sessions
          FROM spend_limit_hits
          WHERE hit_at >= $1::date
          GROUP BY 1
          ORDER BY 1 DESC
        `,
        [sinceDay]
      );
      return (result.rows as Row[]).map((row) => ({
        day: String(row.day),
        daily: Number(row.daily),
        monthly: Number(row.monthly),
        ip: Number(row.ip),
        distinctSessions: Number(row.sessions),
      }));
    },
    async dayStats(day) {
      const spend = await pool.query(
        `SELECT COALESCE(SUM(cost_usd), 0) AS cost, COALESCE(SUM(calls), 0) AS calls FROM model_spend_daily WHERE day = $1`,
        [day]
      );
      const hits = await pool.query(
        `
          SELECT COUNT(*) AS hits, COUNT(DISTINCT session_id) AS sessions
          FROM spend_limit_hits
          WHERE (hit_at AT TIME ZONE 'UTC')::date = $1::date
        `,
        [day]
      );
      return {
        costUsd: Number((spend.rows as Row[])[0]?.cost ?? 0),
        calls: Number((spend.rows as Row[])[0]?.calls ?? 0),
        hits: Number((hits.rows as Row[])[0]?.hits ?? 0),
        distinctSessions: Number((hits.rows as Row[])[0]?.sessions ?? 0),
      };
    },
  };
}

// ---------------------------------------------------------------------------------------------------------------
// State

let testStore: SpendStore | null = null;
let memoryStore = new MemorySpendStore();
let totalsCache: { at: number; day: string; month: string; dayAgg: Agg; monthAgg: Agg } | null = null;
let lastWarnAt = 0;
let lastSeenDay: string | null = null;
const ipWindows = new Map<string, number[]>();

function warnThrottled(message: string, error?: unknown): void {
  const now = Date.now();
  if (now - lastWarnAt >= WARN_INTERVAL_MS) {
    lastWarnAt = now;
    console.warn(`[spend-guard] ${message}; using in-process tally`, error ?? "");
  }
}

async function getStore(): Promise<SpendStore | null> {
  if (testStore) {
    return testStore;
  }
  try {
    const pool = await getPool();
    if (!pool) {
      warnThrottled("no database configured");
      return null;
    }
    await ensureTables(pool);
    return createPostgresStore(pool);
  } catch (error) {
    warnThrottled("database unavailable", error);
    return null;
  }
}

async function loadTotals(now: Date, forceRefresh = false): Promise<{ dayAgg: Agg; monthAgg: Agg }> {
  const day = dayKey(now);
  const month = monthStartKey(now);
  if (!forceRefresh && totalsCache && now.getTime() - totalsCache.at < TOTALS_TTL_MS && totalsCache.day === day && totalsCache.month === month) {
    return totalsCache;
  }

  const next = dayKey(nextMonthStart(now));
  let totals: { day: Agg; month: Agg };
  try {
    const store = await getStore();
    totals = store ? await store.totals(day, month, next) : await memoryStore.totals(day, month, next);
  } catch (error) {
    warnThrottled("could not read spend totals", error);
    totals = await memoryStore.totals(day, month, next);
  }
  totalsCache = { at: now.getTime(), day, month, dayAgg: totals.day, monthAgg: totals.month };
  return totalsCache;
}

// ---------------------------------------------------------------------------------------------------------------
// Daily summary

function maybeEmitDailySummary(now: Date): void {
  const today = dayKey(now);
  const previous = lastSeenDay;
  lastSeenDay = today;
  if (!previous || previous >= today) {
    return;
  }
  void (async () => {
    try {
      const store = await getStore();
      const stats = await (store ?? memoryStore).dayStats(previous).catch(() => memoryStore.dayStats(previous));
      const summary = { day: previous, costUsd: Number(stats.costUsd.toFixed(6)), calls: stats.calls, hits: stats.hits, distinctSessionsBlocked: stats.distinctSessions };
      console.info("[spend-guard] daily summary", JSON.stringify(summary));
      await logSpendDailySummary(summary);
    } catch (error) {
      console.warn("[spend-guard] failed to emit daily summary", error);
    }
  })();
}

// ---------------------------------------------------------------------------------------------------------------
// Public API

/** Adds one finished model call's estimated cost to the day/month tallies. Never throws. */
export async function recordSpend(record: ModelUsageRecord): Promise<void> {
  try {
    const now = new Date();
    maybeEmitDailySummary(now);
    const cost = estimateCostUsd(record);
    const day = dayKey(now);
    await memoryStore.addSpend(day, record.kind, cost);
    if (totalsCache && totalsCache.day === day && totalsCache.month === monthStartKey(now)) {
      totalsCache.dayAgg = { costUsd: totalsCache.dayAgg.costUsd + cost, calls: totalsCache.dayAgg.calls + 1 };
      totalsCache.monthAgg = { costUsd: totalsCache.monthAgg.costUsd + cost, calls: totalsCache.monthAgg.calls + 1 };
    }
    const store = await getStore();
    await store?.addSpend(day, record.kind, cost);
  } catch (error) {
    warnThrottled("could not record spend", error);
  }
}

function limitMessage(limitType: LimitType): string {
  if (limitType === "monthly") {
    return "We've hit this month's analysis limit. Takes that were already generated still work, and new ones come back soon.";
  }
  if (limitType === "ip") {
    return "You've asked for a lot of new takes in the past hour. Takes that were already generated still work, and you can try again in about an hour.";
  }
  return "We've hit today's analysis limit. Takes that were already generated still work, and new ones come back tomorrow.";
}

function retryAfterSeconds(limitType: LimitType, now: Date): number {
  if (limitType === "ip") {
    return 3600;
  }
  const target = limitType === "daily" ? nextDayStart(now) : nextMonthStart(now);
  // Capped at a day so a client never waits longer than a monthly reset can be reconsidered.
  return Math.min(86_400, Math.max(1, Math.ceil((target.getTime() - now.getTime()) / 1000)));
}

function ipOverCap(ip: string, cap: number, nowMs: number): boolean {
  const recent = (ipWindows.get(ip) ?? []).filter((at) => nowMs - at < IP_WINDOW_MS);
  if (recent.length >= cap) {
    ipWindows.set(ip, recent);
    return true;
  }
  return false;
}

function trackIp(ip: string, nowMs: number): void {
  const recent = (ipWindows.get(ip) ?? []).filter((at) => nowMs - at < IP_WINDOW_MS);
  recent.push(nowMs);
  ipWindows.delete(ip); // re-insert so the Map's insertion order approximates least-recently-used
  ipWindows.set(ip, recent);
  while (ipWindows.size > MAX_TRACKED_IPS) {
    const oldest = ipWindows.keys().next().value;
    if (oldest === undefined) {
      break;
    }
    ipWindows.delete(oldest);
  }
}

async function recordHit(hit: HitRow, isAdmin: boolean): Promise<void> {
  console.warn("[spend-guard] limit hit", {
    kind: hit.kind,
    limitType: hit.limitType,
    enforced: hit.enforced,
    daySpendUsd: Number(hit.daySpendUsd.toFixed(4)),
    monthSpendUsd: Number(hit.monthSpendUsd.toFixed(4)),
  });
  await memoryStore.addHit(hit);
  try {
    const store = await getStore();
    await store?.addHit(hit);
  } catch (error) {
    warnThrottled("could not record limit hit", error);
  }
  if (!isAdmin) {
    await logSpendLimitHit(
      {
        kind: hit.kind,
        limitType: hit.limitType,
        enforced: hit.enforced,
        daySpendUsd: Number(hit.daySpendUsd.toFixed(6)),
        monthSpendUsd: Number(hit.monthSpendUsd.toFixed(6)),
      },
      hit.sessionId
    );
  }
}

/**
 * Call before a cache-miss model generation. Throws SpendLimitError when a limit is hit and enforcement is on;
 * in log-only mode it records the would-block and lets the request through. Admin requests are never blocked.
 */
export async function checkBudget(params: { kind: ModelCallKind; ip: string | undefined; isAdmin: boolean; sessionId: string | null | undefined }): Promise<void> {
  const config = readSpendConfig();
  if (config.monthlyBudgetUsd === null || config.dailyCeilingUsd === null) {
    return;
  }
  if (params.isAdmin) {
    return;
  }

  const now = new Date();
  maybeEmitDailySummary(now);
  const { dayAgg, monthAgg } = await loadTotals(now);

  let limitType: LimitType | null = null;
  if (monthAgg.costUsd >= config.monthlyBudgetUsd) {
    limitType = "monthly";
  } else if (dayAgg.costUsd >= config.dailyCeilingUsd) {
    limitType = "daily";
  } else if (config.ipHourlyCap > 0 && params.ip && ipOverCap(params.ip, config.ipHourlyCap, now.getTime())) {
    limitType = "ip";
  }

  if (!limitType) {
    if (config.ipHourlyCap > 0 && params.ip) {
      trackIp(params.ip, now.getTime());
    }
    return;
  }

  // A would-block in log-only mode still counts toward the IP window so the log shows what enforcement would do.
  if (!config.enforce && limitType === "ip" && params.ip) {
    trackIp(params.ip, now.getTime());
  }

  await recordHit(
    {
      hitAt: now,
      kind: params.kind,
      limitType,
      enforced: config.enforce,
      daySpendUsd: dayAgg.costUsd,
      monthSpendUsd: monthAgg.costUsd,
      sessionId: params.sessionId?.trim().slice(0, 128) || null,
    },
    params.isAdmin
  );

  if (config.enforce) {
    throw new SpendLimitError(limitType, retryAfterSeconds(limitType, now), limitMessage(limitType));
  }
}

/** Maps a SpendLimitError to the documented 429 response. */
export function respondSpendLimit(res: Response, error: SpendLimitError): void {
  res.set("Retry-After", String(error.retryAfterSeconds));
  res.status(429).json({ error: error.message, code: error.code });
}

export async function getSpendReport() {
  const config = readSpendConfig();
  const now = new Date();
  const { dayAgg, monthAgg } = await loadTotals(now, true);
  const since = dayKey(new Date(now.getTime() - 6 * 86_400_000));
  let hitsLast7Days: DayHits[];
  try {
    const store = await getStore();
    hitsLast7Days = await (store ?? memoryStore).hitsSince(since);
  } catch (error) {
    warnThrottled("could not read limit hits", error);
    hitsLast7Days = await memoryStore.hitsSince(since);
  }
  return {
    today: dayAgg,
    month: monthAgg,
    budget: config.monthlyBudgetUsd,
    ceiling: config.dailyCeilingUsd,
    ipHourlyCap: config.ipHourlyCap,
    enforce: config.enforce,
    hitsLast7Days,
  };
}

export function __setSpendStoreForTests(store: SpendStore | null): void {
  testStore = store;
}

export function __resetSpendGuardForTests(): void {
  testStore = null;
  memoryStore = new MemorySpendStore();
  totalsCache = null;
  lastWarnAt = 0;
  lastSeenDay = null;
  tablesReady = false;
  ipWindows.clear();
}
