import crypto from "node:crypto";
import type { Pool } from "pg";
import { getPool } from "./db.js";
import type { PlayerType } from "./types.js";

const MAX_MEMORY_MATCHUPS = 10_000;

export interface VoteState {
  /** The player this voter picked, or null if they have not voted. */
  yourVote: string | null;
  /** Votes per player id. Only revealed once the voter has voted, so the split cannot sway the pick. */
  counts: Record<string, number> | null;
  total: number;
}

type VoteStore = {
  /** Records a vote unless this voter already voted on the matchup. */
  cast: (matchupKey: string, voterHash: string, choice: string) => Promise<void>;
  counts: (matchupKey: string) => Promise<Record<string, number>>;
  voteOf: (matchupKey: string, voterHash: string) => Promise<string | null>;
};

let initialized = false;
let testStore: VoteStore | null = null;
const memoryMatchups = new Map<string, Map<string, string>>();

export function buildMatchupKey(type: PlayerType, playerIds: string[]): string {
  return `${type}:${[...playerIds].sort().join("|")}`;
}

// The raw voter id never reaches storage, and it is unrelated to the analytics session id.
function hashVoterId(voterId: string): string {
  return crypto.createHash("sha256").update(`compare-votes-v1:${voterId}`).digest("hex");
}

const memoryStore: VoteStore = {
  async cast(matchupKey, voterHash, choice) {
    let voters = memoryMatchups.get(matchupKey);
    if (!voters) {
      if (memoryMatchups.size >= MAX_MEMORY_MATCHUPS) {
        const oldest = memoryMatchups.keys().next().value;
        if (oldest !== undefined) {
          memoryMatchups.delete(oldest);
        }
      }
      voters = new Map();
      memoryMatchups.set(matchupKey, voters);
    }
    if (!voters.has(voterHash)) {
      voters.set(voterHash, choice);
    }
  },
  async counts(matchupKey) {
    const counts: Record<string, number> = {};
    for (const choice of memoryMatchups.get(matchupKey)?.values() ?? []) {
      counts[choice] = (counts[choice] ?? 0) + 1;
    }
    return counts;
  },
  async voteOf(matchupKey, voterHash) {
    return memoryMatchups.get(matchupKey)?.get(voterHash) ?? null;
  },
};

async function initializeTable(pool: Pool): Promise<void> {
  if (initialized) {
    return;
  }

  await pool.query(`
    CREATE TABLE IF NOT EXISTS compare_votes (
      matchup_key TEXT NOT NULL,
      voter_hash TEXT NOT NULL,
      choice TEXT NOT NULL,
      created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
      PRIMARY KEY (matchup_key, voter_hash)
    )
  `);

  initialized = true;
}

async function getStore(): Promise<VoteStore> {
  if (testStore) {
    return testStore;
  }

  const pool = await getPool();
  if (!pool) {
    return memoryStore;
  }
  await initializeTable(pool);

  return {
    async cast(matchupKey, voterHash, choice) {
      await pool.query(
        `
          INSERT INTO compare_votes (matchup_key, voter_hash, choice)
          VALUES ($1, $2, $3)
          ON CONFLICT (matchup_key, voter_hash) DO NOTHING
        `,
        [matchupKey, voterHash, choice]
      );
    },
    async counts(matchupKey) {
      const result = await pool.query(
        `SELECT choice, COUNT(*)::int AS votes FROM compare_votes WHERE matchup_key = $1 GROUP BY choice`,
        [matchupKey]
      );
      const counts: Record<string, number> = {};
      for (const row of result.rows as { choice: string; votes: number }[]) {
        counts[row.choice] = row.votes;
      }
      return counts;
    },
    async voteOf(matchupKey, voterHash) {
      const result = await pool.query(
        `SELECT choice FROM compare_votes WHERE matchup_key = $1 AND voter_hash = $2`,
        [matchupKey, voterHash]
      );
      return (result.rows[0] as { choice: string } | undefined)?.choice ?? null;
    },
  };
}

async function readState(store: VoteStore, matchupKey: string, voterHash: string): Promise<VoteState> {
  const yourVote = await store.voteOf(matchupKey, voterHash);
  if (!yourVote) {
    return { yourVote: null, counts: null, total: 0 };
  }
  const counts = await store.counts(matchupKey);
  const total = Object.values(counts).reduce((sum, votes) => sum + votes, 0);
  return { yourVote, counts, total };
}

export async function getVoteState(type: PlayerType, playerIds: string[], voterId: string): Promise<VoteState> {
  const store = await getStore();
  return readState(store, buildMatchupKey(type, playerIds), hashVoterId(voterId));
}

/** The first vote from a voter on a matchup stands; later attempts return the original vote. */
export async function castVote(
  type: PlayerType,
  playerIds: string[],
  choice: string,
  voterId: string
): Promise<VoteState> {
  const store = await getStore();
  const matchupKey = buildMatchupKey(type, playerIds);
  const voterHash = hashVoterId(voterId);
  await store.cast(matchupKey, voterHash, choice);
  return readState(store, matchupKey, voterHash);
}

export function __setCompareVoteStoreForTests(store: VoteStore | null): void {
  testStore = store;
}

export function __resetCompareVotesForTests(): void {
  memoryMatchups.clear();
}
