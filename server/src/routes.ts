import express from "express";
import rateLimit from "express-rate-limit";
import { z } from "zod";
import { buildAboutContent, buildAnalysisPrompt, buildComparisonPrompt, generateQuickInsight, recommendBestPlayer } from "./analysis.js";
import { callOpenAiChat } from "./openai.js";
import { getPlayerById, getPlayerSummaries } from "./dataStore.js";
import { logAnalysisEvent, logSessionStart, logShareEvent } from "./analytics.js";
import { AnalysisMode, ANALYSIS_VIBES, DEFAULT_ANALYSIS_MODE } from "./vibes.js";
import { isAdminModeRequest } from "./admin.js";

const analyzeLimiter = rateLimit({
  windowMs: 60_000,
  max: 20,
  standardHeaders: true,
  legacyHeaders: false,
});

const router = express.Router();

const playerTypeSchema = z.enum(["hitter", "pitcher"]);

router.get("/health", (_req, res) => {
  res.json({ status: "ok" });
});

router.post("/api/sessions", async (req, res) => {
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

  await logSessionStart(parseResult.data.sessionId, req.get("referer"));
  res.status(204).end();
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
    const response = await callOpenAiChat(prompt, persona, mode);
    const sessionId = req.get("x-session-id") ?? "";
    if (!isAdminModeRequest(req)) {
      await logAnalysisEvent({
        sessionId,
        playerName: player.Name,
        analysisMode: mode,
        playerType,
        eventType: "single",
        referer: req.get("referer"),
      });
    }
    res.json(response);
  } catch (error) {
    console.error("[api/analyze] OpenAI request failed", error);
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

  try {
    const response = await callOpenAiChat(prompt, persona, mode);

    const sessionId = req.get("x-session-id") ?? "";
    const playerName = players.map((entry) => entry.Name).filter(Boolean).join(" vs ");
    if (!isAdminModeRequest(req)) {
      await logAnalysisEvent({
        sessionId,
        playerName: playerName || players.map((entry) => entry.PlayerId).join(" vs "),
        analysisMode: mode,
        playerType,
        eventType: "compare",
        referer: req.get("referer"),
      });
    }

    res.json(response);
  } catch (error) {
    console.error("[api/compare/analyze] OpenAI request failed", error);
    res.status(502).json({ error: "Analysis service temporarily unavailable" });
  }
});

router.post("/api/share-events", async (req, res) => {
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

  await logShareEvent({
    sessionId: parseResult.data.sessionId,
    playerName: parseResult.data.playerName,
    analysisMode: parseResult.data.analysisMode,
    eventType: parseResult.data.eventType,
    playerType: parseResult.data.playerType,
    shareUrl: parseResult.data.shareUrl,
    referer: req.get("referer"),
  });

  res.status(204).end();
});

router.get("/api/vibes", (_req, res) => {
  const vibes = Object.entries(ANALYSIS_VIBES).map(([id, description]) => {
    const readable = id
      .split("_")
      .map((segment) => segment.charAt(0).toUpperCase() + segment.slice(1))
      .join(" ");

    return {
      id,
      label: id === DEFAULT_ANALYSIS_MODE ? `${readable} (Default)` : readable,
      description,
    };
  });

  res.json({ vibes, defaultMode: DEFAULT_ANALYSIS_MODE });
});

router.get("/api/about", (_req, res) => {
  res.json(buildAboutContent());
});

export default router;
