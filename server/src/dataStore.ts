import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { parse } from "csv-parse/sync";
import { DataFreshness, HitterRecord, PitcherRecord, PlayerLookupRecord, PlayerSummary, PlayerType } from "./types.js";
import { safeNumber } from "./utils.js";

interface PlayerSearchEntry {
  summary: PlayerSummary;
  name: string;
  lastName: string;
}

interface BaseballDataStore {
  hitters: Map<string, HitterRecord>;
  pitchers: Map<string, PitcherRecord>;
  lookup: PlayerLookupRecord[];
  summaries: PlayerSummary[];
  searchIndex: PlayerSearchEntry[];
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

const NAME_SUFFIXES = new Set(["jr", "jr.", "sr", "sr.", "ii", "iii", "iv", "v"]);

// Lowercased once at load so searches don't re-lowercase every name on every keystroke.
function buildSearchEntry(summary: PlayerSummary): PlayerSearchEntry {
  const name = summary.name.toLowerCase();
  const words = name.split(/\s+/).filter(Boolean);
  while (words.length > 1 && NAME_SUFFIXES.has(words[words.length - 1])) {
    words.pop();
  }
  return { summary, name, lastName: words[words.length - 1] ?? name };
}

function isWordChar(char: string | undefined): boolean {
  return char !== undefined && /[\p{L}\p{N}]/u.test(char);
}

// 0: name or last name starts with the query, 1: a later word starts with it, 2: appears mid-word; null: no match.
function matchTier(entry: PlayerSearchEntry, query: string): 0 | 1 | 2 | null {
  const first = entry.name.indexOf(query);
  if (first === -1) {
    return null;
  }
  if (first === 0 || entry.lastName.startsWith(query)) {
    return 0;
  }
  for (let index = first; index !== -1; index = entry.name.indexOf(query, index + 1)) {
    if (!isWordChar(entry.name[index - 1])) {
      return 1;
    }
  }
  return 2;
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
    searchIndex: summaries.map(buildSearchEntry),
    dataFreshness: readDataFreshness(),
  };
}

const store = buildDataStore();

export function getPlayerSummaries(type: PlayerType | "all", query?: string, limit = 50): PlayerSummary[] {
  const targetType = type === "all" ? undefined : type;
  const normalizedQuery = query?.toLowerCase().trim();

  if (!normalizedQuery) {
    return store.summaries.filter((summary) => !targetType || summary.type === targetType).slice(0, limit);
  }

  // Best matches first; file order is kept within each tier.
  const tiers: PlayerSummary[][] = [[], [], []];
  for (const entry of store.searchIndex) {
    if (targetType && entry.summary.type !== targetType) {
      continue;
    }
    const tier = matchTier(entry, normalizedQuery);
    if (tier !== null) {
      tiers[tier].push(entry.summary);
    }
  }
  return tiers.flat().slice(0, limit);
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

export function __setDataFreshnessForTests(dataFreshness: DataFreshness): void {
  store.dataFreshness = dataFreshness;
}
