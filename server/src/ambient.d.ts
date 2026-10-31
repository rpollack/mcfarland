declare module "pg" {
  export interface PoolConfig {
    connectionString?: string;
    ssl?: unknown;
    max?: number;
    idleTimeoutMillis?: number;
    connectionTimeoutMillis?: number;
  }
  export class Pool {
    constructor(config?: PoolConfig);
    query: (text: string, params?: unknown[]) => Promise<{ rows: unknown[]; rowCount: number }>;
    connect: () => Promise<{ query: Pool["query"]; release: () => void }>;
    end: () => Promise<void>;
    on: (event: "error", listener: (err: Error) => void) => Pool;
  }
}

declare namespace Express {
  interface Request {
    isAdminMode?: boolean;
  }
}
