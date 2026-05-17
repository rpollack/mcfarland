import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { parse } from "csv-parse/sync";
import { getDataFreshness, listPlayers } from "./dataStore.js";
import { callOpenAiChat } from "./openai.js";
import type { HitterRecord, PitcherRecord, PlayerType } from "./types.js";
import { DEFAULT_ANALYSIS_MODE, type AnalysisMode } from "./vibes.js";

const currentDir = path.dirname(fileURLToPath(import.meta.url));
const DATA_ROOT = path.resolve(currentDir, "..", "..");
const HITTER_BREAKOUT_PREVIOUS_SEASON_MIN_PA = 300;
const STARTER_BREAKOUT_PREVIOUS_SEASON_MIN_TBF = 350;
const RELIEVER_BREAKOUT_PREVIOUS_SEASON_MIN_TBF = 125;
const STARTER_MIN_PREVIOUS_SEASON_STARTS = 10;
const STARTER_MIN_PREVIOUS_SEASON_START_RATE = 0.5;

type SocialCandidate = {
  playerId: string;
  playerType: PlayerType;
  playerName: string;
  mlbamid?: string | null;
  score: number;
  whyNow: string;
  statSnapshot: string;
  shareUrl: string;
  news: NewsItem[];
};

type NewsItem = {
  title: string;
  link: string;
  source?: string;
  publishedAt?: string;
};

type NewsSearchItem = NewsItem & {
  query: string;
};

type NewsDrivenCandidate = SocialCandidate & {
  newsScore: number;
};

type NewsworthyQuickLinkCache = {
  dataThroughDate: string;
  players: { id: string; name: string; type: PlayerType; mlbamid?: string | null }[];
};

type PreviousHitterSample = {
  plateAppearances: number;
};

type PreviousPitcherSample = {
  battersFaced: number;
  games: number;
  starts: number;
  role: "starter" | "reliever";
};

type PreviousSeasonSampleCache = {
  season: number;
  hitters: Map<string, PreviousHitterSample>;
  pitchers: Map<string, PreviousPitcherSample>;
};

let newsworthyQuickLinkCache: NewsworthyQuickLinkCache | null = null;
let previousSeasonSampleCache: PreviousSeasonSampleCache | null = null;

export type SocialDraftSuggestion = {
  playerId: string;
  playerType: PlayerType;
  playerName: string;
  whyNow: string;
  recommendationWhy: string;
  shareUrl: string;
  statSnapshot: string;
  news: NewsItem[];
};

export type SocialSuggestionResponse = {
  generatedAt: string;
  dataThroughLabel: string;
  candidates: SocialDraftSuggestion[];
  recommended: SocialDraftSuggestion;
  xPosts: string[];
  blueskyPosts: string[];
  modelSummary: string;
  usedFallback: boolean;
};

export type TrendingQuickLinksResponse = {
  generatedAt: string;
  newsworthy: { id: string; name: string; type: PlayerType; mlbamid?: string | null }[];
  hitters: {
    trending: { id: string; name: string; type: "hitter"; mlbamid?: string | null }[];
    breakouts: { id: string; name: string; type: "hitter"; mlbamid?: string | null }[];
  };
  pitchers: {
    trending: { id: string; name: string; type: "pitcher"; mlbamid?: string | null }[];
    breakouts: { id: string; name: string; type: "pitcher"; mlbamid?: string | null }[];
  };
};

function valueOrZero(value: number | null | undefined): number {
  if (value === null || value === undefined || Number.isNaN(value)) {
    return 0;
  }
  return value;
}

function readCsvRows(filename: string): Record<string, string>[] {
  const filePath = path.resolve(DATA_ROOT, filename);
  if (!fs.existsSync(filePath)) {
    return [];
  }

  return parse(fs.readFileSync(filePath, "utf8"), {
    bom: true,
    columns: true,
    skip_empty_lines: true,
    trim: true,
  }) as Record<string, string>[];
}

function parseNumber(value: string | null | undefined): number {
  if (!value) {
    return 0;
  }
  const parsed = Number(value);
  return Number.isFinite(parsed) ? parsed : 0;
}

function getPreviousSeason(): number | null {
  const year = Number(getDataFreshness().dataThroughDate.slice(0, 4));
  return Number.isFinite(year) ? year - 1 : null;
}

function getPitcherPreviousSeasonRole(games: number, starts: number): PreviousPitcherSample["role"] {
  if (starts >= STARTER_MIN_PREVIOUS_SEASON_STARTS) {
    return "starter";
  }
  if (games > 0 && starts / games >= STARTER_MIN_PREVIOUS_SEASON_START_RATE) {
    return "starter";
  }
  return "reliever";
}

function loadPreviousSeasonSamples(): PreviousSeasonSampleCache | null {
  const season = getPreviousSeason();
  if (!season) {
    return null;
  }
  if (previousSeasonSampleCache?.season === season) {
    return previousSeasonSampleCache;
  }

  const hitters = new Map<string, PreviousHitterSample>();
  readCsvRows(`fangraphs-leaderboards-${season}.csv`).forEach((row) => {
    const playerId = row.PlayerId;
    if (!playerId) {
      return;
    }
    hitters.set(String(playerId), {
      plateAppearances: parseNumber(row.PA),
    });
  });

  const pitchers = new Map<string, PreviousPitcherSample>();
  readCsvRows(`pitcher-stats-${season}.csv`).forEach((row) => {
    const playerId = row.PlayerId;
    if (!playerId) {
      return;
    }
    const games = parseNumber(row.G);
    const starts = parseNumber(row.GS);
    pitchers.set(String(playerId), {
      battersFaced: parseNumber(row.TBF),
      games,
      starts,
      role: getPitcherPreviousSeasonRole(games, starts),
    });
  });

  previousSeasonSampleCache = { season, hitters, pitchers };
  return previousSeasonSampleCache;
}

function getPreviousSeasonHitterSample(playerId: string): PreviousHitterSample | null {
  return loadPreviousSeasonSamples()?.hitters.get(playerId) ?? null;
}

function getPreviousSeasonPitcherSample(playerId: string): PreviousPitcherSample | null {
  return loadPreviousSeasonSamples()?.pitchers.get(playerId) ?? null;
}

function getPitcherBreakoutPreviousSeasonThreshold(sample: PreviousPitcherSample): number {
  return sample.role === "starter" ? STARTER_BREAKOUT_PREVIOUS_SEASON_MIN_TBF : RELIEVER_BREAKOUT_PREVIOUS_SEASON_MIN_TBF;
}

function hasQualifiedHitterBreakoutBaseline(player: HitterRecord): boolean {
  const previousSample = getPreviousSeasonHitterSample(String(player.PlayerId));
  return Boolean(previousSample && previousSample.plateAppearances >= HITTER_BREAKOUT_PREVIOUS_SEASON_MIN_PA);
}

function hasQualifiedPitcherBreakoutBaseline(player: PitcherRecord): boolean {
  const previousSample = getPreviousSeasonPitcherSample(String(player.PlayerId));
  return Boolean(previousSample && previousSample.battersFaced >= getPitcherBreakoutPreviousSeasonThreshold(previousSample));
}

export function __clearPreviousSeasonSampleCacheForTests(): void {
  previousSeasonSampleCache = null;
}

export function __getPitcherBreakoutPreviousSeasonThresholdForTests(games: number, starts: number): number {
  const role = getPitcherPreviousSeasonRole(games, starts);
  return role === "starter" ? STARTER_BREAKOUT_PREVIOUS_SEASON_MIN_TBF : RELIEVER_BREAKOUT_PREVIOUS_SEASON_MIN_TBF;
}

export function __getBreakoutBaselineEligibilityForTests(
  playerType: PlayerType,
  playerId: string
): { qualified: boolean; sample: number; threshold: number; role?: PreviousPitcherSample["role"] } | null {
  if (playerType === "hitter") {
    const sample = getPreviousSeasonHitterSample(playerId);
    if (!sample) {
      return null;
    }
    return {
      qualified: sample.plateAppearances >= HITTER_BREAKOUT_PREVIOUS_SEASON_MIN_PA,
      sample: sample.plateAppearances,
      threshold: HITTER_BREAKOUT_PREVIOUS_SEASON_MIN_PA,
    };
  }

  const sample = getPreviousSeasonPitcherSample(playerId);
  if (!sample) {
    return null;
  }
  const threshold = getPitcherBreakoutPreviousSeasonThreshold(sample);
  return {
    qualified: sample.battersFaced >= threshold,
    sample: sample.battersFaced,
    threshold,
    role: sample.role,
  };
}

function absolute(value: number | null | undefined): number {
  if (value === null || value === undefined || Number.isNaN(value)) {
    return 0;
  }
  return Math.abs(value);
}

function sampleWeight(value: number | null | undefined, fullWeightAt: number): number {
  if (value === null || value === undefined || Number.isNaN(value) || value <= 0) {
    return 0.25;
  }
  return Math.max(0.25, Math.min(value / fullWeightAt, 1));
}

function formatSigned(value: number | null | undefined, digits = 3): string {
  if (value === null || value === undefined || Number.isNaN(value)) {
    return "n/a";
  }
  const fixed = Math.abs(value) < 1 ? Number(value).toFixed(digits).replace(/^(-?)0+/, "$1") : Number(value).toFixed(digits);
  return value > 0 ? `+${fixed}` : fixed;
}

function buildShareUrl(baseUrl: string, playerType: PlayerType, playerId: string): string {
  const url = new URL("/share", baseUrl);
  url.searchParams.set("mode", "single");
  url.searchParams.set("playerType", playerType);
  url.searchParams.set("playerId", playerId);
  return url.toString();
}

function buildCandidate(player: HitterRecord | PitcherRecord, baseUrl: string): SocialCandidate {
  return player.player_type === "hitter"
    ? buildHitterCandidate(player as HitterRecord, baseUrl)
    : buildPitcherCandidate(player as PitcherRecord, baseUrl);
}

function buildHitterCandidate(player: HitterRecord, baseUrl: string): SocialCandidate {
  const wobaDiff = player.wOBA_lg_adj_diff ?? player.wOBA_diff;
  const xwobaDiff = player.xwOBA_lg_adj_diff ?? player.xwOBA_diff;
  const slgDiff = player.SLG_lg_adj_diff ?? player.SLG_diff;
  const obpDiff = player.OBP_lg_adj_diff ?? player.OBP_diff;
  const barrelDiff = player.Barrel_pct_lg_adj_diff ?? player.Barrel_pct_diff;
  const kDiff = player.K_pct_lg_adj_diff ?? player.K_pct_diff;
  const bbDiff = player.BB_pct_lg_adj_diff ?? player.BB_pct_diff;
  const score =
    (absolute(wobaDiff) * 6 +
      absolute(xwobaDiff) * 6 +
      absolute(slgDiff) * 4 +
      absolute(obpDiff) * 3 +
      absolute(barrelDiff) * 0.35 +
      absolute(kDiff) * 0.2 +
      absolute(bbDiff) * 0.2) *
    sampleWeight(player.PA_cur, 70);

  const headlineMetric =
    absolute(wobaDiff) >= absolute(slgDiff)
      ? `league-adjusted wOBA ${formatSigned(wobaDiff)}`
      : `league-adjusted SLG ${formatSigned(slgDiff)}`;

  return {
    playerId: String(player.PlayerId),
    playerType: "hitter",
    playerName: player.Name,
    mlbamid: player.mlbamid ?? null,
    score,
    whyNow: `${player.Name} stands out because ${headlineMetric} over ${Math.round(player.PA_cur ?? 0)} PA.`,
    statSnapshot: `${Math.round(player.PA_cur ?? 0)} PA · lg-adj wOBA ${formatSigned(wobaDiff)} · xwOBA ${formatSigned(xwobaDiff)} · SLG ${formatSigned(slgDiff)}`,
    shareUrl: buildShareUrl(baseUrl, "hitter", String(player.PlayerId)),
    news: [],
  };
}

function buildPitcherCandidate(player: PitcherRecord, baseUrl: string): SocialCandidate {
  const eraDiff = player.era_lg_adj_diff ?? player.era_diff;
  const xeraDiff = player.xera_lg_adj_diff ?? player.xera_diff;
  const kMinusBbDiff = player.k_minus_bb_percent_lg_adj_diff ?? player.k_minus_bb_percent_diff;
  const kDiff = player.k_percent_lg_adj_diff ?? player.k_percent_diff;
  const bbDiff = player.bb_percent_lg_adj_diff ?? player.bb_percent_diff;
  const cswDiff = player.csw_percent_lg_adj_diff ?? player.csw_percent_diff;
  const barrelDiff = player.barrel_percent_lg_adj_diff ?? player.barrel_percent_diff;
  const score =
    (absolute(eraDiff) * 5 +
      absolute(xeraDiff) * 5 +
      absolute(kMinusBbDiff) * 0.3 +
      absolute(kDiff) * 0.2 +
      absolute(bbDiff) * 0.2 +
      absolute(cswDiff) * 0.18 +
      absolute(barrelDiff) * 0.2) *
    sampleWeight(player.tbf, 70);

  const headlineMetric =
    absolute(eraDiff) >= absolute(xeraDiff)
      ? `league-adjusted ERA ${formatSigned(eraDiff, 2)}`
      : `league-adjusted xERA ${formatSigned(xeraDiff, 2)}`;

  return {
    playerId: String(player.PlayerId),
    playerType: "pitcher",
    playerName: player.Name,
    mlbamid: player.mlbamid ?? null,
    score,
    whyNow: `${player.Name} stands out because ${headlineMetric} over ${Math.round(player.tbf ?? 0)} TBF.`,
    statSnapshot: `${Math.round(player.tbf ?? 0)} TBF · lg-adj ERA ${formatSigned(eraDiff, 2)} · xERA ${formatSigned(xeraDiff, 2)} · K-BB% ${formatSigned(kMinusBbDiff, 1)}`,
    shareUrl: buildShareUrl(baseUrl, "pitcher", String(player.PlayerId)),
    news: [],
  };
}

function countImprovedSignals(signals: boolean[]): number {
  return signals.filter(Boolean).length;
}

function buildHitterBreakoutCandidate(player: HitterRecord, baseUrl: string): SocialCandidate | null {
  const plateAppearances = valueOrZero(player.PA_cur);
  const xwobaDiff = valueOrZero(player.xwOBA_lg_adj_diff ?? player.xwOBA_diff);
  const barrelDiff = valueOrZero(player.Barrel_pct_lg_adj_diff ?? player.Barrel_pct_diff);
  const bbDiff = valueOrZero(player.BB_pct_lg_adj_diff ?? player.BB_pct_diff);
  const kDiff = valueOrZero(player.K_pct_lg_adj_diff ?? player.K_pct_diff);
  const slgDiff = valueOrZero(player.SLG_lg_adj_diff ?? player.SLG_diff);
  const obpDiff = valueOrZero(player.OBP_lg_adj_diff ?? player.OBP_diff);
  const babipDiff = valueOrZero(player.BABIP_lg_adj_diff ?? player.BABIP_diff);
  const wobaDiff = valueOrZero(player.wOBA_lg_adj_diff ?? player.wOBA_diff);

  if (!hasQualifiedHitterBreakoutBaseline(player) || plateAppearances < 25 || (xwobaDiff <= 0 && barrelDiff <= 0)) {
    return null;
  }

  const improvedSignals = countImprovedSignals([
    xwobaDiff > 0.015,
    barrelDiff > 1.5,
    bbDiff > 1,
    kDiff < -1,
  ]);

  if (improvedSignals < 2) {
    return null;
  }

  const skillGain = xwobaDiff * 40 + barrelDiff * 1.8 + bbDiff * 1.2 + (-kDiff) * 1.0;
  const support = slgDiff * 12 + obpDiff * 10 + (barrelDiff > 0 && xwobaDiff > 0 ? 4 : 0) + (bbDiff > 0 && kDiff < 0 ? 3 : 0);
  const luckPenalty = Math.max(0, babipDiff - 0.02) * 25 + Math.max(0, wobaDiff - xwobaDiff) * 35;
  const reliability = sampleWeight(plateAppearances, 120);
  const score = (skillGain + support - luckPenalty) * reliability;

  if (score <= 0) {
    return null;
  }

  return {
    playerId: String(player.PlayerId),
    playerType: "hitter",
    playerName: player.Name,
    mlbamid: player.mlbamid ?? null,
    score,
    whyNow: `${player.Name} looks like a real early breakout: league-adjusted xwOBA ${formatSigned(xwobaDiff)} and Barrel% ${formatSigned(barrelDiff, 1)} over ${Math.round(plateAppearances)} PA.`,
    statSnapshot: `${Math.round(plateAppearances)} PA · lg-adj xwOBA ${formatSigned(xwobaDiff)} · Barrel% ${formatSigned(barrelDiff, 1)} · BB% ${formatSigned(bbDiff, 1)}`,
    shareUrl: buildShareUrl(baseUrl, "hitter", String(player.PlayerId)),
    news: [],
  };
}

function buildPitcherBreakoutCandidate(player: PitcherRecord, baseUrl: string): SocialCandidate | null {
  const battersFaced = valueOrZero(player.tbf);
  const xeraDiff = valueOrZero(player.xera_lg_adj_diff ?? player.xera_diff);
  const kMinusBbDiff = valueOrZero(player.k_minus_bb_percent_lg_adj_diff ?? player.k_minus_bb_percent_diff);
  const kDiff = valueOrZero(player.k_percent_lg_adj_diff ?? player.k_percent_diff);
  const bbDiff = valueOrZero(player.bb_percent_lg_adj_diff ?? player.bb_percent_diff);
  const cswDiff = valueOrZero(player.csw_percent_lg_adj_diff ?? player.csw_percent_diff);
  const barrelDiff = valueOrZero(player.barrel_percent_lg_adj_diff ?? player.barrel_percent_diff);
  const eraDiff = valueOrZero(player.era_lg_adj_diff ?? player.era_diff);
  const babipDiff = valueOrZero(player.babip_lg_adj_diff ?? player.babip_diff);
  const lobDiff = valueOrZero(player.lob_percent_lg_adj_diff ?? player.lob_percent_diff);

  if (!hasQualifiedPitcherBreakoutBaseline(player) || battersFaced < 30 || (xeraDiff >= 0 && kMinusBbDiff <= 0)) {
    return null;
  }

  const improvedSignals = countImprovedSignals([
    xeraDiff < -0.2,
    kMinusBbDiff > 2,
    bbDiff < -1,
    cswDiff > 1,
    barrelDiff < -1,
  ]);

  if (improvedSignals < 2) {
    return null;
  }

  const skillGain =
    (-xeraDiff) * 18 +
    kMinusBbDiff * 1.8 +
    kDiff * 0.9 +
    (-bbDiff) * 1.1 +
    cswDiff * 0.8 +
    (-barrelDiff) * 1.0;
  const support = (kMinusBbDiff > 0 && xeraDiff < 0 ? 4 : 0) + (cswDiff > 0 && bbDiff < 0 ? 3 : 0);
  const luckPenalty =
    Math.max(0, xeraDiff - eraDiff) * 10 +
    Math.max(0, -babipDiff - 0.015) * 8 +
    Math.max(0, lobDiff - 4) * 0.8;
  const reliability = sampleWeight(battersFaced, 120);
  const score = (skillGain + support - luckPenalty) * reliability;

  if (score <= 0) {
    return null;
  }

  return {
    playerId: String(player.PlayerId),
    playerType: "pitcher",
    playerName: player.Name,
    mlbamid: player.mlbamid ?? null,
    score,
    whyNow: `${player.Name} has the look of a real pitching breakout: league-adjusted xERA ${formatSigned(xeraDiff, 2)} and K-BB% ${formatSigned(kMinusBbDiff, 1)} over ${Math.round(battersFaced)} TBF.`,
    statSnapshot: `${Math.round(battersFaced)} TBF · lg-adj xERA ${formatSigned(xeraDiff, 2)} · K-BB% ${formatSigned(kMinusBbDiff, 1)} · CSW% ${formatSigned(cswDiff, 1)}`,
    shareUrl: buildShareUrl(baseUrl, "pitcher", String(player.PlayerId)),
    news: [],
  };
}

function topTrendingCandidates(type: PlayerType, baseUrl: string): SocialCandidate[] {
  if (type === "hitter") {
    return listPlayers("hitter")
      .map((player) => buildHitterCandidate(player as HitterRecord, baseUrl))
      .sort((a, b) => b.score - a.score)
      .slice(0, 3);
  }

  return listPlayers("pitcher")
    .map((player) => buildPitcherCandidate(player as PitcherRecord, baseUrl))
    .sort((a, b) => b.score - a.score)
    .slice(0, 3);
}

function topBreakoutCandidates(type: PlayerType, baseUrl: string): SocialCandidate[] {
  if (type === "hitter") {
    return listPlayers("hitter")
      .map((player) => buildHitterBreakoutCandidate(player as HitterRecord, baseUrl))
      .filter((candidate): candidate is SocialCandidate => Boolean(candidate))
      .sort((a, b) => b.score - a.score)
      .slice(0, 3);
  }

  return listPlayers("pitcher")
    .map((player) => buildPitcherBreakoutCandidate(player as PitcherRecord, baseUrl))
    .filter((candidate): candidate is SocialCandidate => Boolean(candidate))
    .sort((a, b) => b.score - a.score)
    .slice(0, 3);
}

function decodeHtml(value: string): string {
  return value
    .replace(/<!\[CDATA\[([\s\S]*?)\]\]>/g, "$1")
    .replace(/&amp;/g, "&")
    .replace(/&quot;/g, "\"")
    .replace(/&#39;/g, "'")
    .replace(/&lt;/g, "<")
    .replace(/&gt;/g, ">")
    .trim();
}

function extractTag(block: string, tag: string): string | undefined {
  const match = block.match(new RegExp(`<${tag}(?:\\s[^>]*)?>([\\s\\S]*?)<\\/${tag}>`, "i"));
  return match?.[1] ? decodeHtml(match[1]) : undefined;
}

function parseNewsItems(xml: string, query: string, limit: number): NewsSearchItem[] {
  return [...xml.matchAll(/<item>([\s\S]*?)<\/item>/gi)]
    .slice(0, limit)
    .map((match) => {
      const block = match[1];
      return {
        title: extractTag(block, "title") ?? "",
        link: extractTag(block, "link") ?? "",
        source: extractTag(block, "source"),
        publishedAt: extractTag(block, "pubDate"),
        query,
      } satisfies NewsSearchItem;
    })
    .filter((item) => item.title && item.link);
}

async function fetchGoogleNewsRss(query: string, limit: number): Promise<NewsSearchItem[]> {
  if (process.env.NODE_ENV === "test") {
    return [];
  }

  const url = `https://news.google.com/rss/search?q=${encodeURIComponent(query)}&hl=en-US&gl=US&ceid=US:en`;

  try {
    const response = await fetch(url, {
      headers: {
        "User-Agent": "McFARLAND Social Assistant/1.0",
      },
    });

    if (!response.ok) {
      return [];
    }

    const xml = await response.text();
    return parseNewsItems(xml, query, limit);
  } catch (_error) {
    return [];
  }
}

async function fetchPlayerNews(playerName: string): Promise<NewsItem[]> {
  return fetchGoogleNewsRss(`"${playerName}" MLB OR baseball`, 3);
}

const NEWS_DISCOVERY_QUERIES = [
  "MLB breakout player",
  "MLB hot streak player",
  "MLB slump player",
  "MLB rookie debut",
  "MLB call-up prospect",
  "MLB walk-off homer",
  "MLB strikeouts pitcher",
  "fantasy baseball riser",
] as const;

function normalizeSearchText(value: string): string {
  return value
    .toLowerCase()
    .normalize("NFD")
    .replace(/[\u0300-\u036f]/g, "")
    .replace(/[^a-z0-9]+/g, " ")
    .trim();
}

function containsName(text: string, playerName: string): boolean {
  const normalizedText = ` ${normalizeSearchText(text)} `;
  const normalizedName = normalizeSearchText(playerName);
  if (!normalizedName.includes(" ")) {
    return false;
  }
  return normalizedText.includes(` ${normalizedName} `);
}

function eventSpecificityScore(item: NewsSearchItem): number {
  const text = normalizeSearchText(`${item.query} ${item.title}`);
  const eventTerms = [
    "breakout",
    "hot streak",
    "slump",
    "rookie",
    "debut",
    "call up",
    "prospect",
    "walk off",
    "homer",
    "strikeouts",
    "riser",
    "sleeper",
    "waiver",
  ];
  return eventTerms.filter((term) => text.includes(term)).length;
}

function freshnessScore(publishedAt?: string): number {
  if (!publishedAt) {
    return 0.5;
  }
  const publishedTime = Date.parse(publishedAt);
  if (Number.isNaN(publishedTime)) {
    return 0.5;
  }
  const ageHours = Math.max(0, (Date.now() - publishedTime) / 36e5);
  if (ageHours <= 24) return 3;
  if (ageHours <= 72) return 2;
  if (ageHours <= 168) return 1;
  return 0.25;
}

function sourceKeyFor(item: NewsItem): string {
  if (item.source) {
    return normalizeSearchText(item.source);
  }
  try {
    return normalizeSearchText(new URL(item.link).hostname);
  } catch {
    return "unknown";
  }
}

function buildPlayerIndex(baseUrl: string): SocialCandidate[] {
  return [
    ...listPlayers("hitter").map((player) => buildCandidate(player as HitterRecord, baseUrl)),
    ...listPlayers("pitcher").map((player) => buildCandidate(player as PitcherRecord, baseUrl)),
  ];
}

export function buildNewsDrivenCandidates(
  newsItems: NewsSearchItem[],
  playerIndex: SocialCandidate[]
): NewsDrivenCandidate[] {
  const byPlayer = new Map<string, { candidate: SocialCandidate; news: NewsSearchItem[]; score: number; sources: Set<string> }>();
  const sourceUseCount = new Map<string, number>();

  for (const item of newsItems) {
    const sourceKey = sourceKeyFor(item);
    if ((sourceUseCount.get(sourceKey) ?? 0) >= 8) {
      continue;
    }

    const matchedPlayers = playerIndex.filter((candidate) => containsName(item.title, candidate.playerName)).slice(0, 3);
    if (matchedPlayers.length === 0) {
      continue;
    }

    sourceUseCount.set(sourceKey, (sourceUseCount.get(sourceKey) ?? 0) + 1);

    for (const candidate of matchedPlayers) {
      const key = `${candidate.playerType}:${candidate.playerId}`;
      const existing = byPlayer.get(key) ?? {
        candidate,
        news: [],
        score: 0,
        sources: new Set<string>(),
      };

      if (existing.sources.has(sourceKey)) {
        continue;
      }

      existing.sources.add(sourceKey);
      if (existing.news.length < 3) {
        existing.news.push(item);
      }
      existing.score += 5 + eventSpecificityScore(item) * 1.5 + freshnessScore(item.publishedAt);
      byPlayer.set(key, existing);
    }
  }

  return [...byPlayer.values()]
    .map(({ candidate, news, score, sources }) => {
      const sourceDiversity = sources.size;
      const mcfarlandSignal = Math.min(Math.log1p(Math.max(candidate.score, 0)) * 2, 8);
      const newsScore = score + sourceDiversity * 4 + mcfarlandSignal;
      return {
        ...candidate,
        score: newsScore,
        newsScore,
        news: news.map(({ query: _query, ...item }) => item),
        whyNow: `${candidate.playerName} is showing up in today's MLB news cycle, and McFARLAND adds this angle: ${candidate.whyNow}`,
      };
    })
    .sort((a, b) => b.newsScore - a.newsScore);
}

type NewsDrivenCandidatePoolFetcher = (baseUrl: string) => Promise<SocialCandidate[]>;

let newsDrivenCandidatePoolFetcher: NewsDrivenCandidatePoolFetcher | null = null;

async function fetchNewsDrivenCandidatePool(baseUrl: string): Promise<SocialCandidate[]> {
  if (newsDrivenCandidatePoolFetcher) {
    return newsDrivenCandidatePoolFetcher(baseUrl);
  }

  const feeds = await Promise.all(
    NEWS_DISCOVERY_QUERIES.map((query) => fetchGoogleNewsRss(query, 10))
  );
  const newsCandidates = buildNewsDrivenCandidates(feeds.flat(), buildPlayerIndex(baseUrl));
  return newsCandidates.slice(0, 10);
}

function candidateKey(candidate: SocialCandidate): string {
  return `${candidate.playerType}:${candidate.playerId}`;
}

function supplementCandidatePool(newsDrivenCandidates: SocialCandidate[], statCandidates: SocialCandidate[]): SocialCandidate[] {
  const seen = new Set(newsDrivenCandidates.map(candidateKey));
  const supplements = statCandidates.filter((candidate) => {
    const key = candidateKey(candidate);
    if (seen.has(key)) {
      return false;
    }
    seen.add(key);
    return true;
  });

  return [...newsDrivenCandidates, ...supplements].slice(0, 10);
}

function buildPrompt(candidates: SocialCandidate[]): string {
  const freshness = getDataFreshness();
  const candidateLines = candidates.map((candidate, index) => {
    const newsLines =
      candidate.news.length > 0
        ? candidate.news
            .map((item) => `  - ${item.title}${item.source ? ` (${item.source})` : ""}`)
            .join("\n")
        : "  - No strong news headline found";

    return [
      `${index + 1}. ${candidate.playerName} [${candidate.playerType}]`,
      `why_now: ${candidate.whyNow}`,
      `stat_snapshot: ${candidate.statSnapshot}`,
      `share_url: ${candidate.shareUrl}`,
      "recent_news:",
      newsLines,
    ].join("\n");
  });

  return [
    "You are helping a solo builder decide what baseball player analysis to post on social media today.",
    `Data is current through games on ${freshness.dataThroughLabel}.`,
    "",
    "Choose exactly 3 players from the news-first candidate list below who are best for a timely social post today.",
    "Prioritize players whose news hook and McFARLAND angle work together; do not reward fame or big-market volume by itself.",
    "Recommend exactly 1 of the 3 as the best post today and explain why it is better than the others.",
    "Then draft 2 X posts and 2 Bluesky posts for the recommended player.",
    "Use the provided share_url in every draft.",
    "Keep the posts punchy, non-cringe, and written by a baseball fan with taste. No hashtags unless absolutely necessary.",
    "Return valid JSON only with this exact shape:",
    '{"candidates":[{"playerId":"...","playerType":"hitter|pitcher","playerName":"...","whyNow":"...","recommendationWhy":"...","shareUrl":"...","statSnapshot":"..."}],"recommendedPlayerId":"...","xPosts":["...","..."],"blueskyPosts":["...","..."],"modelSummary":"..."}',
    "",
    "Candidate list:",
    ...candidateLines,
  ].join("\n");
}

function parseJsonPayload(text: string): any | null {
  const trimmed = text.trim();
  const direct = trimmed.match(/^\{[\s\S]*\}$/);
  if (direct) {
    try {
      return JSON.parse(direct[0]);
    } catch (_error) {
      return null;
    }
  }

  const block = trimmed.match(/```json\s*([\s\S]*?)```/i) ?? trimmed.match(/```([\s\S]*?)```/);
  if (!block) {
    return null;
  }

  try {
    return JSON.parse(block[1]);
  } catch (_error) {
    return null;
  }
}

function buildFallbackResponse(candidates: SocialCandidate[]): SocialSuggestionResponse {
  const chosen = candidates.slice(0, 3);
  const recommended = chosen[0];
  const fallbackCandidates: SocialDraftSuggestion[] = chosen.map((candidate, index) => ({
    playerId: candidate.playerId,
    playerType: candidate.playerType,
    playerName: candidate.playerName,
    whyNow: candidate.whyNow,
    recommendationWhy:
      index === 0
        ? "Best mix of strong current signal, enough sample to matter, and a clear angle for a social post."
        : "Still timely, but a little less obvious or urgent than the top recommendation.",
    shareUrl: candidate.shareUrl,
    statSnapshot: candidate.statSnapshot,
    news: candidate.news,
  }));

  const xPosts = [
    `${recommended.playerName} is a good “is this real?” post today. ${recommended.statSnapshot}. ${recommended.shareUrl}`,
    `If you need one McFARLAND link to post today, it’s ${recommended.playerName}. ${recommended.whyNow} ${recommended.shareUrl}`,
  ];
  const blueskyPosts = [
    `${recommended.playerName} feels like the cleanest baseball post today. ${recommended.statSnapshot}. McFARLAND link: ${recommended.shareUrl}`,
    `${recommended.playerName}: timely, debatable, and worth a closer look. ${recommended.whyNow} ${recommended.shareUrl}`,
  ];

  return {
    generatedAt: new Date().toISOString(),
    dataThroughLabel: getDataFreshness().dataThroughLabel,
    candidates: fallbackCandidates,
    recommended: fallbackCandidates[0],
    xPosts,
    blueskyPosts,
    modelSummary: "Fallback ranking based on current McFARLAND signals, sample size, and any available recent headlines.",
    usedFallback: true,
  };
}

export async function generateSocialSuggestions(baseUrl: string): Promise<SocialSuggestionResponse> {
  const newsDrivenCandidates = await fetchNewsDrivenCandidatePool(baseUrl);

  const hitterCandidates = listPlayers("hitter")
    .map((player) => buildHitterCandidate(player as HitterRecord, baseUrl))
    .sort((a, b) => b.score - a.score)
    .slice(0, 8);

  const pitcherCandidates = listPlayers("pitcher")
    .map((player) => buildPitcherCandidate(player as PitcherRecord, baseUrl))
    .sort((a, b) => b.score - a.score)
    .slice(0, 8);

  const statCandidates = [...hitterCandidates, ...pitcherCandidates]
    .sort((a, b) => b.score - a.score)
    .slice(0, 10);
  const candidatePool = newsDrivenCandidates.length > 0
    ? supplementCandidatePool(newsDrivenCandidates, statCandidates)
    : statCandidates;

  const enriched = await Promise.all(
    candidatePool.map(async (candidate) => ({
      ...candidate,
      news: candidate.news.length > 0 ? candidate.news : await fetchPlayerNews(candidate.playerName),
    }))
  );

  const fallback = buildFallbackResponse(enriched);
  const prompt = buildPrompt(enriched);
  const model = await callOpenAiChat(prompt, "Internal social assistant", DEFAULT_ANALYSIS_MODE as AnalysisMode);
  const parsed = parseJsonPayload(model.analysis);

  if (!parsed || !Array.isArray(parsed.candidates) || !Array.isArray(parsed.xPosts) || !Array.isArray(parsed.blueskyPosts)) {
    return fallback;
  }

  const selectedCandidates = parsed.candidates
    .map((candidate: any) =>
      enriched.find(
        (entry) =>
          entry.playerId === String(candidate.playerId) &&
          entry.playerType === candidate.playerType
      )
    )
    .filter((entry: SocialCandidate | undefined): entry is SocialCandidate => Boolean(entry))
    .slice(0, 3);

  if (selectedCandidates.length === 0) {
    return fallback;
  }

  const hydratedCandidates: SocialDraftSuggestion[] = selectedCandidates.map((candidate: SocialCandidate) => {
    const modelCandidate = parsed.candidates.find((entry: any) => String(entry.playerId) === candidate.playerId && entry.playerType === candidate.playerType) ?? {};
    return {
      playerId: candidate.playerId,
      playerType: candidate.playerType,
      playerName: candidate.playerName,
      whyNow: String(modelCandidate.whyNow ?? candidate.whyNow),
      recommendationWhy: String(modelCandidate.recommendationWhy ?? ""),
      shareUrl: candidate.shareUrl,
      statSnapshot: candidate.statSnapshot,
      news: candidate.news,
    };
  });

  const recommended =
    hydratedCandidates.find((candidate) => candidate.playerId === String(parsed.recommendedPlayerId)) ??
    hydratedCandidates[0];

  return {
    generatedAt: new Date().toISOString(),
    dataThroughLabel: getDataFreshness().dataThroughLabel,
    candidates: hydratedCandidates,
    recommended,
    xPosts: parsed.xPosts.map((entry: unknown) => String(entry)).slice(0, 2),
    blueskyPosts: parsed.blueskyPosts.map((entry: unknown) => String(entry)).slice(0, 2),
    modelSummary: String(parsed.modelSummary ?? "AI-ranked from current McFARLAND signals plus lightweight news context."),
    usedFallback: false,
  };
}

function toTrendPlayer(candidate: SocialCandidate): { id: string; name: string; type: PlayerType; mlbamid?: string | null } {
  return {
    id: candidate.playerId,
    name: candidate.playerName,
    type: candidate.playerType,
    mlbamid: candidate.mlbamid ?? null,
  };
}

async function getDailyNewsworthyQuickLinks(baseUrl: string): Promise<{ id: string; name: string; type: PlayerType; mlbamid?: string | null }[]> {
  const { dataThroughDate } = getDataFreshness();
  if (newsworthyQuickLinkCache?.dataThroughDate === dataThroughDate) {
    return newsworthyQuickLinkCache.players;
  }

  const players = (await fetchNewsDrivenCandidatePool(baseUrl))
    .slice(0, 3)
    .map(toTrendPlayer);

  newsworthyQuickLinkCache = {
    dataThroughDate,
    players,
  };

  return players;
}

export async function getTrendingQuickLinks(baseUrl: string): Promise<TrendingQuickLinksResponse> {
  const newsworthy = await getDailyNewsworthyQuickLinks(baseUrl);

  const hitters = topTrendingCandidates("hitter", baseUrl).map((candidate) => ({
    id: candidate.playerId,
    name: candidate.playerName,
    type: "hitter" as const,
    mlbamid: candidate.mlbamid ?? null,
  }));
  const hitterBreakouts = topBreakoutCandidates("hitter", baseUrl).map((candidate) => ({
    id: candidate.playerId,
    name: candidate.playerName,
    type: "hitter" as const,
    mlbamid: candidate.mlbamid ?? null,
  }));

  const pitchers = topTrendingCandidates("pitcher", baseUrl).map((candidate) => ({
    id: candidate.playerId,
    name: candidate.playerName,
    type: "pitcher" as const,
    mlbamid: candidate.mlbamid ?? null,
  }));
  const pitcherBreakouts = topBreakoutCandidates("pitcher", baseUrl).map((candidate) => ({
    id: candidate.playerId,
    name: candidate.playerName,
    type: "pitcher" as const,
    mlbamid: candidate.mlbamid ?? null,
  }));

  return {
    generatedAt: new Date().toISOString(),
    newsworthy,
    hitters: { trending: hitters, breakouts: hitterBreakouts },
    pitchers: { trending: pitchers, breakouts: pitcherBreakouts },
  };
}

export function __clearNewsworthyQuickLinkCacheForTests(): void {
  newsworthyQuickLinkCache = null;
}

export function __setNewsDrivenCandidatePoolFetcherForTests(fetcher: NewsDrivenCandidatePoolFetcher | null): void {
  newsDrivenCandidatePoolFetcher = fetcher;
}
