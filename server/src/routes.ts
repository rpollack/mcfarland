import express from "express";
import rateLimit from "express-rate-limit";
import { z } from "zod";
import { buildAboutContent, buildAnalysisPrompt, buildComparisonPrompt, generateQuickInsight, recommendBestPlayer } from "./analysis.js";
import { callOpenAiChat } from "./openai.js";
import { getPlayerById, getPlayerSummaries, listPlayers } from "./dataStore.js";
import { logAnalysisEvent, logSessionStart, logShareEvent } from "./analytics.js";
import { AnalysisMode, ANALYSIS_VIBES, DEFAULT_ANALYSIS_MODE } from "./vibes.js";
import { isAdminModeRequest } from "./admin.js";
import { HitterRecord, PitcherRecord } from "./types.js";

const analyzeLimiter = rateLimit({
  windowMs: 60_000,
  max: 120,
  standardHeaders: true,
  legacyHeaders: false,
});

const router = express.Router();

const playerTypeSchema = z.enum(["hitter", "pitcher"]);

type TrendPlayer = {
  id: string;
  name: string;
  type: "hitter" | "pitcher";
  mlbamid?: string | null;
};

type ScoredTrendPlayer = TrendPlayer & {
  score: number;
};

function toFinite(value: number | null | undefined): number {
  return typeof value === "number" && Number.isFinite(value) ? value : 0;
}

function scoreHitterTrend(player: HitterRecord): number | null {
  if ((player.PA_cur ?? 0) < 15) {
    return null;
  }

  return (
    toFinite(player.wOBA_diff) * 520 +
    toFinite(player.xwOBA_diff) * 340 +
    toFinite(player.SLG_diff) * 170 +
    toFinite(player.OBP_diff) * 140 +
    toFinite(player.AVG_diff) * 230 +
    toFinite(player.Barrel_pct_diff) * 3.4 +
    toFinite(player.BB_pct_diff) * 1.8 -
    toFinite(player.K_pct_diff) * 1.6
  );
}

function scorePitcherTrend(player: PitcherRecord): number | null {
  if ((player.tbf ?? 0) < 20) {
    return null;
  }

  return (
    -toFinite(player.era_diff) * 17 +
    -toFinite(player.xera_diff) * 12 +
    toFinite(player.k_minus_bb_percent_diff) * 1.9 +
    toFinite(player.k_percent_diff) * 1.1 -
    toFinite(player.bb_percent_diff) * 1.2 -
    toFinite(player.barrel_percent_diff) * 1.4 +
    toFinite(player.csw_percent_diff) * 0.9 -
    toFinite(player.babip_diff) * 14
  );
}

function getTopRisersAndFallers(entries: ScoredTrendPlayer[]): { risers: TrendPlayer[]; fallers: TrendPlayer[] } {
  const risers = [...entries]
    .sort((a, b) => b.score - a.score)
    .slice(0, 3)
    .map(({ score: _score, ...player }) => player);
  const riserIds = new Set(risers.map((player) => player.id));
  const fallers = [...entries]
    .sort((a, b) => a.score - b.score)
    .filter((player) => !riserIds.has(player.id))
    .slice(0, 3)
    .map(({ score: _score, ...player }) => player);

  return { risers, fallers };
}

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

router.get("/api/trends/weekly", (_req, res) => {
  const hitterEntries: ScoredTrendPlayer[] = listPlayers("hitter")
    .map((entry) => entry as HitterRecord)
    .flatMap((player) => {
      const score = scoreHitterTrend(player);
      if (score === null) {
        return [];
      }
      return [{
        id: player.PlayerId,
        name: player.Name,
        type: "hitter" as const,
        mlbamid: player.mlbamid ?? null,
        score,
      }];
    });

  const pitcherEntries: ScoredTrendPlayer[] = listPlayers("pitcher")
    .map((entry) => entry as PitcherRecord)
    .flatMap((player) => {
      const score = scorePitcherTrend(player);
      if (score === null) {
        return [];
      }
      return [{
        id: player.PlayerId,
        name: player.Name,
        type: "pitcher" as const,
        mlbamid: player.mlbamid ?? null,
        score,
      }];
    });

  const hitters = getTopRisersAndFallers(hitterEntries);
  const pitchers = getTopRisersAndFallers(pitcherEntries);

  res.json({
    generatedAt: new Date().toISOString(),
    hitters,
    pitchers,
  });
});

export default router;
