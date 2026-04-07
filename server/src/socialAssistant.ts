import { getDataFreshness, listPlayers } from "./dataStore.js";
import { callOpenAiChat } from "./openai.js";
import type { HitterRecord, PitcherRecord, PlayerType } from "./types.js";
import { DEFAULT_ANALYSIS_MODE, type AnalysisMode } from "./vibes.js";

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
  hitters: {
    trending: { id: string; name: string; type: "hitter"; mlbamid?: string | null }[];
  };
  pitchers: {
    trending: { id: string; name: string; type: "pitcher"; mlbamid?: string | null }[];
  };
};

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

function buildHitterCandidate(player: HitterRecord, baseUrl: string): SocialCandidate {
  const score =
    (absolute(player.wOBA_diff) * 6 +
      absolute(player.xwOBA_diff) * 6 +
      absolute(player.SLG_diff) * 4 +
      absolute(player.OBP_diff) * 3 +
      absolute(player.Barrel_pct_diff) * 0.35 +
      absolute(player.K_pct_diff) * 0.2 +
      absolute(player.BB_pct_diff) * 0.2) *
    sampleWeight(player.PA_cur, 70);

  const headlineMetric =
    absolute(player.wOBA_diff) >= absolute(player.SLG_diff)
      ? `wOBA ${formatSigned(player.wOBA_diff)} vs recent baseline`
      : `SLG ${formatSigned(player.SLG_diff)} vs recent baseline`;

  return {
    playerId: String(player.PlayerId),
    playerType: "hitter",
    playerName: player.Name,
    mlbamid: player.mlbamid ?? null,
    score,
    whyNow: `${player.Name} stands out because ${headlineMetric} over ${Math.round(player.PA_cur ?? 0)} PA.`,
    statSnapshot: `${Math.round(player.PA_cur ?? 0)} PA · wOBA ${formatSigned(player.wOBA_diff)} · xwOBA ${formatSigned(player.xwOBA_diff)} · SLG ${formatSigned(player.SLG_diff)}`,
    shareUrl: buildShareUrl(baseUrl, "hitter", String(player.PlayerId)),
    news: [],
  };
}

function buildPitcherCandidate(player: PitcherRecord, baseUrl: string): SocialCandidate {
  const score =
    (absolute(player.era_diff) * 5 +
      absolute(player.xera_diff) * 5 +
      absolute(player.k_minus_bb_percent_diff) * 0.3 +
      absolute(player.k_percent_diff) * 0.2 +
      absolute(player.bb_percent_diff) * 0.2 +
      absolute(player.csw_percent_diff) * 0.18 +
      absolute(player.barrel_percent_diff) * 0.2) *
    sampleWeight(player.tbf, 70);

  const headlineMetric =
    absolute(player.era_diff) >= absolute(player.xera_diff)
      ? `ERA ${formatSigned(player.era_diff, 2)} vs recent baseline`
      : `xERA ${formatSigned(player.xera_diff, 2)} vs recent baseline`;

  return {
    playerId: String(player.PlayerId),
    playerType: "pitcher",
    playerName: player.Name,
    mlbamid: player.mlbamid ?? null,
    score,
    whyNow: `${player.Name} stands out because ${headlineMetric} over ${Math.round(player.tbf ?? 0)} TBF.`,
    statSnapshot: `${Math.round(player.tbf ?? 0)} TBF · ERA ${formatSigned(player.era_diff, 2)} · xERA ${formatSigned(player.xera_diff, 2)} · K-BB% ${formatSigned(player.k_minus_bb_percent_diff, 1)}`,
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

async function fetchPlayerNews(playerName: string): Promise<NewsItem[]> {
  if (process.env.NODE_ENV === "test") {
    return [];
  }

  const query = encodeURIComponent(`"${playerName}" MLB OR baseball`);
  const url = `https://news.google.com/rss/search?q=${query}&hl=en-US&gl=US&ceid=US:en`;

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
    const items = [...xml.matchAll(/<item>([\s\S]*?)<\/item>/gi)]
      .slice(0, 3)
      .map((match) => {
        const block = match[1];
        return {
          title: extractTag(block, "title") ?? "",
          link: extractTag(block, "link") ?? "",
          source: extractTag(block, "source"),
          publishedAt: extractTag(block, "pubDate"),
        } satisfies NewsItem;
      })
      .filter((item) => item.title && item.link);

    return items;
  } catch (_error) {
    return [];
  }
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
    "Choose exactly 3 players from the candidate list below who are best for a timely social post today.",
    "Prioritize players who feel timely because of a feat, hot start, cold start, notable team context, or interesting overreaction potential.",
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
  const hitterCandidates = listPlayers("hitter")
    .map((player) => buildHitterCandidate(player as HitterRecord, baseUrl))
    .sort((a, b) => b.score - a.score)
    .slice(0, 8);

  const pitcherCandidates = listPlayers("pitcher")
    .map((player) => buildPitcherCandidate(player as PitcherRecord, baseUrl))
    .sort((a, b) => b.score - a.score)
    .slice(0, 8);

  const candidatePool = [...hitterCandidates, ...pitcherCandidates]
    .sort((a, b) => b.score - a.score)
    .slice(0, 10);

  const enriched = await Promise.all(
    candidatePool.map(async (candidate) => ({
      ...candidate,
      news: await fetchPlayerNews(candidate.playerName),
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

export function getTrendingQuickLinks(baseUrl: string): TrendingQuickLinksResponse {
  const hitters = topTrendingCandidates("hitter", baseUrl).map((candidate) => ({
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

  return {
    generatedAt: new Date().toISOString(),
    hitters: { trending: hitters },
    pitchers: { trending: pitchers },
  };
}
