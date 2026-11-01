import express from "express";
import rateLimit from "express-rate-limit";
import { z } from "zod";
import { buildAboutContent, buildAnalysisPrompt, buildComparisonPrompt, generateQuickInsight, recommendBestPlayer } from "./analysis.js";
import { buildFantasyDailyMatchupPrompt, parseFantasyCachedPayload, serializeFantasyCachedPayload } from "./fantasy.js";
import { getDailyMatchupContext } from "./mlbMatchups.js";
import { ANALYSIS_UNAVAILABLE_HEADLINE, callClaudeAnalysis, callClaudeFantasyDecision, isCacheableFantasyDecision, isCacheableAnalysisResponse } from "./claude.js";
import { buildAnalysisCacheKey, getCachedAnalysis, saveCachedAnalysis } from "./analysisCache.js";
import { getDataFreshness, getPlayerById, getPlayerSummaries } from "./dataStore.js";
import { castVote, getVoteState } from "./compareVotes.js";
import { buildMirageBoard, buildMirageBoardPrompt } from "./mirageBoard.js";
import { findRelatedPlayers } from "./relatedPlayers.js";
import { logAnalysisEvent, logAnnouncementEvent, logSessionStart, logShareEvent } from "./analytics.js";
import { AnalysisMode, ANALYSIS_VIBE_LABELS, ANALYSIS_VIBES, DEFAULT_ANALYSIS_MODE } from "./vibes.js";
import { getAdminPassword, isAdminModeRequest } from "./admin.js";
import { getWeeklyTrends, refreshDailyTrendData } from "./weeklyTrends.js";
import { generateSocialSuggestions, getTrendingQuickLinks } from "./socialAssistant.js";

const analyzeLimiter = rateLimit({
  windowMs: 60_000,
  max: 120,
  standardHeaders: true,
  legacyHeaders: false,
});

const lightLimiter = () =>
  rateLimit({
    windowMs: 60_000,
    max: 60,
    standardHeaders: true,
    legacyHeaders: false,
  });
const trendingLimiter = lightLimiter();
const eventLimiter = lightLimiter();
const voteLimiter = lightLimiter();

const adminLimiter = rateLimit({
  windowMs: 60_000,
  max: 10,
  standardHeaders: true,
  legacyHeaders: false,
});

const router = express.Router();

const playerTypeSchema = z.enum(["hitter", "pitcher"]);

async function getAnalysisResponse(params: {
  prompt: string;
  persona: string;
  mode: AnalysisMode;
  playerType: "hitter" | "pitcher";
  playerIds: string[];
}) {
  const freshness = getDataFreshness();
  const cacheKey = buildAnalysisCacheKey(params.mode, params.prompt);
  const cached = await getCachedAnalysis({ cacheKey, dataThroughDate: freshness.dataThroughDate });
  if (cached) {
    return {
      prompt: params.prompt,
      persona: params.persona,
      headline: cached.headline,
      analysis: cached.analysis,
      cached: true,
    };
  }

  const response = await callClaudeAnalysis(params.prompt, params.persona, params.mode);
  if (isCacheableAnalysisResponse(response)) {
    void saveCachedAnalysis({
      cacheKey,
      dataThroughDate: freshness.dataThroughDate,
      analysisMode: params.mode,
      playerType: params.playerType,
      playerIds: params.playerIds,
      headline: response.headline,
      analysis: response.analysis,
    });
  }

  return response;
}

router.get("/health", (_req, res) => {
  res.json({ status: "ok" });
});

router.post("/api/sessions", eventLimiter, async (req, res) => {
  const schema = z.object({
    sessionId: z.string().min(1).max(128),
  });

  const parseResult = schema.safeParse(req.body);
  if (!parseResult.success) {
    return res.status(400).json({ error: "Invalid request body" });
  }

  if (isAdminModeRequest(req)) {
    return res.status(204).end();
  }

  res.status(204).end();
  void logSessionStart(parseResult.data.sessionId, req.get("referer"));
});

router.get("/api/players", (req, res) => {
  const schema = z.object({
    type: playerTypeSchema.default("hitter"),
    q: z.string().optional(),
    limit: z.coerce.number().int().min(1).max(200).optional(),
  });

  const parseResult = schema.safeParse(req.query);
  if (!parseResult.success) {
    return res.status(400).json({ error: "Invalid query parameters" });
  }

  const { type, q, limit } = parseResult.data;
  const players = getPlayerSummaries(type, q, limit ?? 50);
  res.json({ players });
});

router.get("/api/players/:playerId", (req, res) => {
  const schema = z.object({
    type: playerTypeSchema,
  });
  const parseResult = schema.safeParse(req.query);
  if (!parseResult.success) {
    return res.status(400).json({ error: "Player type is required" });
  }

  const { type } = parseResult.data;
  const { playerId } = req.params;
  const player = getPlayerById(type, playerId);

  if (!player) {
    return res.status(404).json({ error: "Player not found" });
  }

  const quickInsight = generateQuickInsight(player, type);
  res.json({ player, quickInsight });
});

router.get("/api/players/:playerId/related", (req, res) => {
  const parseResult = z.object({ type: playerTypeSchema }).safeParse(req.query);
  if (!parseResult.success) {
    return res.status(400).json({ error: "Player type is required" });
  }

  const related = findRelatedPlayers(parseResult.data.type, req.params.playerId);
  if (!related) {
    return res.status(404).json({ error: "Player not found" });
  }

  res.json({ related });
});

router.get("/api/mirage-board", (req, res) => {
  const parseResult = z.object({ type: playerTypeSchema.default("hitter") }).safeParse(req.query);
  if (!parseResult.success) {
    return res.status(400).json({ error: "Invalid query parameters" });
  }

  res.json({ board: buildMirageBoard(parseResult.data.type) });
});

router.post("/api/mirage-board/intro", analyzeLimiter, async (req, res) => {
  const parseResult = z
    .object({
      playerType: playerTypeSchema,
      analysisMode: z.string().default(DEFAULT_ANALYSIS_MODE),
    })
    .safeParse(req.body);
  if (!parseResult.success) {
    return res.status(400).json({ error: "Invalid request body" });
  }

  const { playerType, analysisMode } = parseResult.data;
  const board = buildMirageBoard(playerType);
  if (board.lucky.length === 0 && board.unlucky.length === 0) {
    return res.status(404).json({ error: "Not enough data for a mirage board yet" });
  }

  const mode = (analysisMode in ANALYSIS_VIBES ? analysisMode : DEFAULT_ANALYSIS_MODE) as AnalysisMode;

  try {
    const response = await getAnalysisResponse({
      prompt: buildMirageBoardPrompt(board),
      persona: ANALYSIS_VIBES[mode],
      mode,
      playerType,
      playerIds: [...board.lucky, ...board.unlucky].map((row) => row.id),
    });
    // The board is still useful without its intro, so a missing intro is an error, not text to display.
    if (response.headline === ANALYSIS_UNAVAILABLE_HEADLINE) {
      return res.status(502).json({ error: "Analysis service temporarily unavailable" });
    }
    res.json(response);
  } catch (error) {
    console.error("[api/mirage-board/intro] Anthropic request failed", error);
    res.status(502).json({ error: "Analysis service temporarily unavailable" });
  }
});

router.get("/api/data-freshness", (_req, res) => {
  res.json(getDataFreshness());
});

router.post("/api/analyze", analyzeLimiter, async (req, res) => {
  const schema = z.object({
    playerId: z.string(),
    playerType: playerTypeSchema,
    analysisMode: z.string().default(DEFAULT_ANALYSIS_MODE),
  });

  const parseResult = schema.safeParse(req.body);
  if (!parseResult.success) {
    return res.status(400).json({ error: "Invalid request body" });
  }

  const { playerId, playerType, analysisMode } = parseResult.data;
  const player = getPlayerById(playerType, playerId);

  if (!player) {
    return res.status(404).json({ error: "Player not found" });
  }

  const mode = (analysisMode in ANALYSIS_VIBES ? analysisMode : DEFAULT_ANALYSIS_MODE) as AnalysisMode;
  const prompt = buildAnalysisPrompt(player, playerType, mode);
  const persona = ANALYSIS_VIBES[mode];

  try {
    const response = await getAnalysisResponse({
      prompt,
      persona,
      mode,
      playerType,
      playerIds: [playerId],
    });
    const sessionId = req.get("x-session-id") ?? "";
    res.json(response);
    if (!isAdminModeRequest(req)) {
      void logAnalysisEvent({
        sessionId,
        playerName: player.Name,
        analysisMode: mode,
        playerType,
        eventType: "single",
        referer: req.get("referer"),
      });
    }
  } catch (error) {
    console.error("[api/analyze] Anthropic request failed", error);
    res.status(502).json({ error: "Analysis service temporarily unavailable" });
  }
});

router.post("/api/compare", (req, res) => {
  const schema = z.object({
    playerType: playerTypeSchema,
    playerIds: z.array(z.string()).min(2).max(3),
  });

  const parseResult = schema.safeParse(req.body);
  if (!parseResult.success) {
    return res.status(400).json({ error: "Invalid request body" });
  }

  const { playerType, playerIds } = parseResult.data;
  const players = playerIds
    .map((id) => getPlayerById(playerType, id))
    .filter((player): player is NonNullable<typeof player> => Boolean(player));

  const recommendedPlayerId = recommendBestPlayer(players, playerType);

  res.json({ players, recommendedPlayerId });
});

const voteIdsSchema = z
  .array(z.string().min(1).max(64))
  .min(2)
  .max(3)
  .refine((ids) => new Set(ids).size === ids.length, "Players must be different");
const voterIdSchema = z.string().min(16).max(128);

router.get("/api/compare/votes", voteLimiter, async (req, res) => {
  const parseResult = z
    .object({
      type: playerTypeSchema,
      ids: z.string().transform((value) => value.split(",").map((id) => id.trim()).filter(Boolean)).pipe(voteIdsSchema),
      voterId: voterIdSchema,
    })
    .safeParse({ type: req.query.type, ids: req.query.ids, voterId: req.get("x-voter-id") });
  if (!parseResult.success) {
    return res.status(400).json({ error: "Invalid vote lookup" });
  }

  const { type, ids, voterId } = parseResult.data;
  if (!ids.every((id) => getPlayerById(type, id))) {
    return res.status(404).json({ error: "Player not found" });
  }

  try {
    res.json(await getVoteState(type, ids, voterId));
  } catch (error) {
    console.error("[api/compare/votes] lookup failed", error);
    res.status(503).json({ error: "Voting is temporarily unavailable" });
  }
});

router.post("/api/compare/votes", voteLimiter, async (req, res) => {
  const parseResult = z
    .object({
      playerType: playerTypeSchema,
      playerIds: voteIdsSchema,
      choice: z.string().min(1).max(64),
      voterId: voterIdSchema,
    })
    .safeParse({ ...req.body, voterId: req.get("x-voter-id") });
  if (!parseResult.success) {
    return res.status(400).json({ error: "Invalid vote" });
  }

  const { playerType, playerIds, choice, voterId } = parseResult.data;
  if (!playerIds.includes(choice)) {
    return res.status(400).json({ error: "Vote must be for one of the compared players" });
  }
  if (!playerIds.every((id) => getPlayerById(playerType, id))) {
    return res.status(404).json({ error: "Player not found" });
  }

  try {
    res.json(await castVote(playerType, playerIds, choice, voterId));
  } catch (error) {
    console.error("[api/compare/votes] vote failed", error);
    res.status(503).json({ error: "Voting is temporarily unavailable" });
  }
});

router.post("/api/compare/analyze", analyzeLimiter, async (req, res) => {
  const schema = z.object({
    playerType: playerTypeSchema,
    playerIds: z.array(z.string()).min(2).max(3),
    analysisMode: z.string().default(DEFAULT_ANALYSIS_MODE),
  });

  const parseResult = schema.safeParse(req.body);
  if (!parseResult.success) {
    return res.status(400).json({ error: "Invalid request body" });
  }

  const { playerType, playerIds, analysisMode } = parseResult.data;
  const players = playerIds
    .map((id) => getPlayerById(playerType, id))
    .filter((player): player is NonNullable<typeof player> => Boolean(player));

  if (players.length === 0) {
    return res.status(404).json({ error: "Players not found" });
  }

  const mode = (analysisMode in ANALYSIS_VIBES ? analysisMode : DEFAULT_ANALYSIS_MODE) as AnalysisMode;
  const prompt = buildComparisonPrompt(players, playerType, mode);
  const persona = ANALYSIS_VIBES[mode];
  const recommendedPlayerId = recommendBestPlayer(players, playerType);

  try {
    const response = await getAnalysisResponse({
      prompt,
      persona,
      mode,
      playerType,
      playerIds,
    });

    const sessionId = req.get("x-session-id") ?? "";
    const playerName = players.map((entry) => entry.Name).filter(Boolean).join(" vs ");
    res.json({
      ...response,
      players,
      recommendedPlayerId,
    });
    if (!isAdminModeRequest(req)) {
      void logAnalysisEvent({
        sessionId,
        playerName: playerName || players.map((entry) => entry.PlayerId).join(" vs "),
        analysisMode: mode,
        playerType,
        eventType: "compare",
        referer: req.get("referer"),
      });
    }
  } catch (error) {
    console.error("[api/compare/analyze] Anthropic request failed", error);
    res.status(502).json({ error: "Analysis service temporarily unavailable" });
  }
});

router.post("/api/fantasy/daily-matchup/analyze", analyzeLimiter, async (req, res) => {
  const schema = z.object({
    playerId: z.string(),
    playerType: playerTypeSchema,
    date: z.string().regex(/^\d{4}-\d{2}-\d{2}$/).optional(),
  });

  const parseResult = schema.safeParse(req.body);
  if (!parseResult.success) {
    return res.status(400).json({ error: "Invalid request body" });
  }

  const { playerId, playerType, date } = parseResult.data;
  const player = getPlayerById(playerType, playerId);
  if (!player) {
    return res.status(404).json({ error: "Player not found" });
  }

  try {
    const matchup = await getDailyMatchupContext({ player, playerType, date });
    const prompt = buildFantasyDailyMatchupPrompt(player, playerType, matchup);
    const cacheMode = "rotisserie_expert" as AnalysisMode;
    const freshness = getDataFreshness();
    const cacheKey = buildAnalysisCacheKey(cacheMode, prompt);
    const cached = await getCachedAnalysis({ cacheKey, dataThroughDate: freshness.dataThroughDate });
    if (cached) {
      const cachedDecision = parseFantasyCachedPayload(cached.headline, cached.analysis);
      if (cachedDecision) {
        const sessionId = req.get("x-session-id") ?? "";
        res.json({
          player,
          matchup,
          ...cachedDecision,
          prompt,
          cached: true,
        });
        if (!isAdminModeRequest(req)) {
          void logAnalysisEvent({
            sessionId,
            playerId,
            playerName: player.Name,
            analysisMode: "fantasy",
            playerType,
            eventType: "fantasy",
            referer: req.get("referer"),
          });
        }
        return;
      }
    }

    const decision = await callClaudeFantasyDecision(prompt);
    if (isCacheableFantasyDecision(decision)) {
      void saveCachedAnalysis({
        cacheKey,
        dataThroughDate: freshness.dataThroughDate,
        analysisMode: cacheMode,
        playerType,
        playerIds: [playerId],
        headline: decision.headline,
        analysis: serializeFantasyCachedPayload({
          decision: decision.decision,
          confidence: decision.confidence,
          analysis: decision.analysis,
        }),
      });
    }

    const sessionId = req.get("x-session-id") ?? "";
    res.json({
      player,
      matchup,
      decision: decision.decision,
      confidence: decision.confidence,
      headline: decision.headline,
      analysis: decision.analysis,
      prompt,
      cached: decision.cached,
    });
    if (!isAdminModeRequest(req)) {
      void logAnalysisEvent({
        sessionId,
        playerId,
        playerName: player.Name,
        analysisMode: "fantasy",
        playerType,
        eventType: "fantasy",
        referer: req.get("referer"),
      });
    }
  } catch (error) {
    console.error("[api/fantasy/daily-matchup/analyze] failed", error);
    res.status(502).json({ error: "Fantasy matchup service temporarily unavailable" });
  }
});

router.post("/api/share-events", eventLimiter, async (req, res) => {
  const schema = z.object({
    sessionId: z.string().min(1).max(128),
    playerName: z.string().min(1).max(256),
    analysisMode: z.string().min(1).max(64),
    eventType: z.string().min(1).max(64),
    playerType: playerTypeSchema.optional(),
    shareUrl: z.string().max(2048).optional(),
  });

  const parseResult = schema.safeParse(req.body);
  if (!parseResult.success) {
    return res.status(400).json({ error: "Invalid request body" });
  }

  res.status(204).end();
  void logShareEvent({
    sessionId: parseResult.data.sessionId,
    playerName: parseResult.data.playerName,
    analysisMode: parseResult.data.analysisMode,
    eventType: parseResult.data.eventType,
    playerType: parseResult.data.playerType,
    shareUrl: parseResult.data.shareUrl,
    referer: req.get("referer"),
  });
});

router.post("/api/announcement-events", eventLimiter, async (req, res) => {
  const schema = z.object({
    sessionId: z.string().min(1).max(128),
    announcementId: z.string().min(1).max(128),
    action: z.enum(["viewed", "clicked", "dismissed"]),
  });

  const parseResult = schema.safeParse(req.body);
  if (!parseResult.success) {
    return res.status(400).json({ error: "Invalid request body" });
  }

  res.status(204).end();
  void logAnnouncementEvent({
    sessionId: parseResult.data.sessionId,
    announcementId: parseResult.data.announcementId,
    action: parseResult.data.action,
    referer: req.get("referer"),
  });
});

router.get("/api/vibes", (_req, res) => {
  const mappedVibes = Object.entries(ANALYSIS_VIBES).map(([id, description]) => {
    const readable = ANALYSIS_VIBE_LABELS[id as AnalysisMode];

    return {
      id,
      label: id === DEFAULT_ANALYSIS_MODE ? `${readable} (Default)` : readable,
      description,
    };
  });
  const [defaultVibe, ...otherVibes] = mappedVibes;
  const vibes = [
    defaultVibe,
    ...otherVibes.sort((a, b) => a.label.localeCompare(b.label)),
  ];

  res.json({ vibes, defaultMode: DEFAULT_ANALYSIS_MODE });
});

router.get("/api/about", (_req, res) => {
  res.json(buildAboutContent());
});

router.get("/api/trending", trendingLimiter, async (req, res) => {
  const baseUrl = `${req.protocol}://${req.get("host")}`;
  res.json(await getTrendingQuickLinks(baseUrl));
});

router.get("/api/admin/social-suggestions", adminLimiter, async (req, res) => {
  const adminPassword = getAdminPassword();
  if (!adminPassword && process.env.NODE_ENV === "production") {
    return res.status(503).json({ error: "Admin access is not configured" });
  }
  if (adminPassword && !isAdminModeRequest(req)) {
    return res.status(403).json({ error: "Admin access required" });
  }

  try {
    const baseUrl = `${req.protocol}://${req.get("host")}`;
    const suggestions = await generateSocialSuggestions(baseUrl);
    return res.json(suggestions);
  } catch (error) {
    console.error("[api/admin/social-suggestions] failed to generate suggestions", error);
    return res.status(500).json({ error: "Unable to generate social suggestions" });
  }
});

router.get("/api/trends/weekly", async (_req, res) => {
  try {
    const trends = await getWeeklyTrends();
    res.json(trends);
  } catch (error) {
    console.error("[api/trends/weekly] failed to generate weekly trends", error);
    res.status(500).json({ error: "Unable to generate weekly trends" });
  }
});

router.post("/api/trends/refresh", async (req, res) => {
  const configuredSecret = process.env.TRENDS_REFRESH_SECRET?.trim();
  if (!configuredSecret) {
    return res.status(503).json({ error: "Trend refresh secret is not configured" });
  }

  const providedSecret = req.get("x-trends-refresh-secret")?.trim();
  if (providedSecret !== configuredSecret) {
    return res.status(403).json({ error: "Forbidden" });
  }

  try {
    const refreshResult = await refreshDailyTrendData();
    if (!refreshResult) {
      return res.status(503).json({ error: "Trend refresh storage is unavailable" });
    }

    res.json({
      status: "ok",
      snapshotDate: refreshResult.snapshotDate,
      rankingsDate: refreshResult.rankingsDate,
    });
  } catch (error) {
    console.error("[api/trends/refresh] failed to refresh trends", error);
    res.status(500).json({ error: "Unable to refresh trends" });
  }
});

export default router;
