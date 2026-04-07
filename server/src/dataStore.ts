import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { parse } from "csv-parse/sync";
import { DataFreshness, HitterRecord, PitcherRecord, PlayerLookupRecord, PlayerSummary, PlayerType } from "./types.js";
import { safeNumber } from "./utils.js";

interface BaseballDataStore {
  hitters: Map<string, HitterRecord>;
  pitchers: Map<string, PitcherRecord>;
  lookup: PlayerLookupRecord[];
  summaries: PlayerSummary[];
  dataFreshness: DataFreshness;
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

function formatDataThroughLabel(dataThroughDate: string): string {
  const parsed = new Date(`${dataThroughDate}T12:00:00Z`);
  if (Number.isNaN(parsed.getTime())) {
    return dataThroughDate;
  }

  return new Intl.DateTimeFormat("en-US", {
    month: "long",
    day: "numeric",
    timeZone: "UTC",
  }).format(parsed);
}

function buildFallbackDataFreshness(): DataFreshness {
  const filenames = ["full_stats_hitters.csv", "full_stats_pitchers.csv", "player_lookup.csv"];
  const latestTimestamp = filenames
    .map((filename) => fs.statSync(path.resolve(DATA_ROOT, filename)).mtimeMs)
    .reduce((max, value) => Math.max(max, value), 0);

  const latestInChicago = new Date(
    new Intl.DateTimeFormat("en-CA", {
      timeZone: "America/Chicago",
      year: "numeric",
      month: "2-digit",
      day: "2-digit",
    }).format(new Date(latestTimestamp))
  );
  latestInChicago.setDate(latestInChicago.getDate() - 1);
  const dataThroughDate = latestInChicago.toISOString().slice(0, 10);

  return {
    dataThroughDate,
    dataThroughLabel: formatDataThroughLabel(dataThroughDate),
  };
}

function readDataFreshness(): DataFreshness {
  const filePath = path.resolve(DATA_ROOT, "data_freshness.json");
  if (!fs.existsSync(filePath)) {
    return buildFallbackDataFreshness();
  }

  try {
    const parsed = JSON.parse(fs.readFileSync(filePath, "utf8")) as { data_through_date?: string };
    if (typeof parsed.data_through_date !== "string" || !parsed.data_through_date) {
      return buildFallbackDataFreshness();
    }

    return {
      dataThroughDate: parsed.data_through_date,
      dataThroughLabel: formatDataThroughLabel(parsed.data_through_date),
    };
  } catch {
    return buildFallbackDataFreshness();
  }
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
    dataFreshness: readDataFreshness(),
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

export function getDataFreshness(): DataFreshness {
  return store.dataFreshness;
}
