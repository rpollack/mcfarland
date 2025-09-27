import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { parse } from "csv-parse/sync";
import { HitterRecord, PitcherRecord, PlayerLookupRecord, PlayerSummary, PlayerType } from "./types.js";
import { safeNumber } from "./utils.js";

interface BaseballDataStore {
  hitters: Map<string, HitterRecord>;
  pitchers: Map<string, PitcherRecord>;
  lookup: PlayerLookupRecord[];
  summaries: PlayerSummary[];
}

const currentDir = path.dirname(fileURLToPath(import.meta.url));
const DATA_ROOT = path.resolve(currentDir, "..", "..");

function readCsv<T>(filename: string): T[] {
  const filePath = path.resolve(DATA_ROOT, filename);
  const fileContents = fs.readFileSync(filePath, "utf8");
  const records = parse(fileContents, {
    columns: true,
    skip_empty_lines: true,
    trim: true,
  }) as Record<string, string>[];

  return records.map((row) => {
    const parsedRow: Record<string, unknown> = {};
    Object.entries(row).forEach(([key, value]) => {
      if (value === undefined) {
        parsedRow[key] = null;
      } else if (key === "Name" || key === "PlayerId" || key === "player_type" || key === "display_name" || key === "position" || key === "mlbamid") {
        parsedRow[key] = value;
      } else {
        const num = safeNumber(value);
        parsedRow[key] = num;
      }
    });
    return parsedRow as T;
  });
}

function buildDataStore(): BaseballDataStore {
  const hitters = readCsv<HitterRecord>("full_stats_hitters.csv");
  const pitchers = readCsv<PitcherRecord>("full_stats_pitchers.csv");
  const lookup = readCsv<PlayerLookupRecord>("player_lookup.csv");

  const hittersMap = new Map<string, HitterRecord>();
  const pitchersMap = new Map<string, PitcherRecord>();

  hitters.forEach((hitter) => {
    hittersMap.set(String(hitter.PlayerId), hitter);
  });

  pitchers.forEach((pitcher) => {
    pitchersMap.set(String(pitcher.PlayerId), pitcher);
  });

  const summaries: PlayerSummary[] = [
    ...hitters.map((hitter) => ({
      id: String(hitter.PlayerId),
      name: hitter.Name,
      type: "hitter" as PlayerType,
      mlbamid: hitter.mlbamid ?? null,
    })),
    ...pitchers.map((pitcher) => ({
      id: String(pitcher.PlayerId),
      name: pitcher.Name,
      type: "pitcher" as PlayerType,
      mlbamid: pitcher.mlbamid ?? null,
    })),
  ];

  return {
    hitters: hittersMap,
    pitchers: pitchersMap,
    lookup,
    summaries,
  };
}

const store = buildDataStore();

export function getPlayerSummaries(type: PlayerType | "all", query?: string, limit = 50): PlayerSummary[] {
  const targetType = type === "all" ? undefined : type;
  const normalizedQuery = query?.toLowerCase().trim();

  return store.summaries
    .filter((summary) => {
      if (targetType && summary.type !== targetType) {
        return false;
      }
      if (!normalizedQuery) {
        return true;
      }
      return summary.name.toLowerCase().includes(normalizedQuery);
    })
    .slice(0, limit);
}

export function getPlayerById(type: PlayerType, playerId: string): HitterRecord | PitcherRecord | null {
  if (type === "hitter") {
    return store.hitters.get(playerId) ?? null;
  }
  return store.pitchers.get(playerId) ?? null;
}

export function listPlayers(type: PlayerType): (HitterRecord | PitcherRecord)[] {
  return type === "hitter" ? Array.from(store.hitters.values()) : Array.from(store.pitchers.values());
}

export function resolvePlayerType(playerId: string): PlayerType | null {
  if (store.hitters.has(playerId)) {
    return "hitter";
  }
  if (store.pitchers.has(playerId)) {
    return "pitcher";
  }
  return null;
}

export function getLookup(): PlayerLookupRecord[] {
  return store.lookup;
}
