import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

const poolInstances: Array<{
  config: Record<string, unknown>;
  query: ReturnType<typeof vi.fn>;
  end: ReturnType<typeof vi.fn>;
  on: ReturnType<typeof vi.fn>;
}> = [];
let queryImplementation: () => Promise<unknown> = async () => ({ rows: [], rowCount: 0 });

vi.mock("pg", () => ({
  Pool: class {
    config: Record<string, unknown>;
    query = vi.fn(() => queryImplementation());
    end = vi.fn(async () => undefined);
    on = vi.fn(() => this);
    constructor(config: Record<string, unknown>) {
      this.config = config;
      poolInstances.push(this as unknown as (typeof poolInstances)[number]);
    }
  },
}));

async function loadGetPool() {
  vi.resetModules();
  return (await import("../src/db.js")).getPool;
}

describe("getPool", () => {
  beforeEach(() => {
    poolInstances.length = 0;
    queryImplementation = async () => ({ rows: [], rowCount: 0 });
  });

  afterEach(() => {
    vi.unstubAllEnvs();
  });

  it("returns null without a database url", async () => {
    vi.stubEnv("DATABASE_URL", "");
    const getPool = await loadGetPool();

    expect(await getPool()).toBeNull();
    expect(poolInstances).toHaveLength(0);
  });

  it("builds one bounded pool with an idle error handler and reuses it", async () => {
    vi.stubEnv("DATABASE_URL", "postgres://user:pw@localhost:5432/db");
    vi.stubEnv("NODE_ENV", "test");
    const getPool = await loadGetPool();

    const first = await getPool();
    const second = await getPool();

    expect(first).toBe(second);
    expect(poolInstances).toHaveLength(1);
    expect(poolInstances[0].config).toMatchObject({
      connectionString: "postgres://user:pw@localhost:5432/db",
      ssl: undefined,
      max: 5,
      idleTimeoutMillis: 30_000,
      connectionTimeoutMillis: 5_000,
    });
    expect(poolInstances[0].on).toHaveBeenCalledWith("error", expect.any(Function));
  });

  it("uses relaxed ssl for render hosts and in production", async () => {
    vi.stubEnv("DATABASE_URL", "postgres://user:pw@db.render.com/db");
    vi.stubEnv("NODE_ENV", "test");
    await (await loadGetPool())();
    vi.stubEnv("DATABASE_URL", "postgres://user:pw@localhost/db");
    vi.stubEnv("NODE_ENV", "production");
    await (await loadGetPool())();

    expect(poolInstances[0].config.ssl).toEqual({ rejectUnauthorized: false });
    expect(poolInstances[1].config.ssl).toEqual({ rejectUnauthorized: false });
  });

  it("retries after a failed initial connection instead of caching the failure", async () => {
    vi.stubEnv("DATABASE_URL", "postgres://user:pw@localhost:5432/db");
    const getPool = await loadGetPool();

    queryImplementation = async () => {
      throw new Error("connection refused");
    };
    await expect(getPool()).rejects.toThrow("connection refused");
    expect(poolInstances[0].end).toHaveBeenCalled();

    queryImplementation = async () => ({ rows: [], rowCount: 0 });
    const pool = await getPool();

    expect(pool).not.toBeNull();
    expect(poolInstances).toHaveLength(2);
  });
});
