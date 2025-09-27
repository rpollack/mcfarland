declare module "pg" {
  export interface PoolConfig {
    connectionString?: string;
    ssl?: unknown;
  }
  export class Pool {
    constructor(config?: PoolConfig);
    query: (text: string, params?: unknown[]) => Promise<{ rows: unknown[] }>;
    connect: () => Promise<{ query: Pool["query"]; release: () => void }>;
    end: () => Promise<void>;
  }
}

declare module "sqlite3" {
  type StatementCallback = (err: Error | null) => void;
  export function verbose(): void;
  export class Database {
    constructor(filename: string);
    run(sql: string, params: unknown[], callback: StatementCallback): void;
  }
}

declare namespace Express {
  interface Request {
    isAdminMode?: boolean;
  }
}
