import "dotenv/config";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import express from "express";
import helmet from "helmet";
import cors from "cors";
import router from "./routes.js";
import { initializeAnalytics } from "./analytics.js";
import { adminModeMiddleware } from "./admin.js";
import { getPlayerById } from "./dataStore.js";
import { ANALYSIS_VIBES, DEFAULT_ANALYSIS_MODE } from "./vibes.js";
import type { PlayerType } from "./types.js";

const currentDir = path.dirname(fileURLToPath(import.meta.url));
const APP_NAME = "McFARLAND";
const DEFAULT_TITLE = "McFARLAND | Advanced Baseball Analysis";
const DEFAULT_DESCRIPTION = "Advanced baseball analysis in plain English. Compare players, explore trends, and share insights.";
const MLB_PHOTO_BASE =
  "https://img.mlbstatic.com/mlb-photos/image/upload/w_1200,d_people:generic:headshot:silo:current.png,q_auto:best,f_auto/v1/people";
const MLB_GENERIC_HEADSHOT = `${MLB_PHOTO_BASE}/0/headshot/67/current`;

type CardMetadata = {
  title: string;
  description: string;
  imageUrl?: string;
  canonicalUrl: string;
};

function getQueryValue(value: unknown): string | undefined {
  if (typeof value === "string") {
    return value;
  }
  if (Array.isArray(value) && typeof value[0] === "string") {
    return value[0];
  }
  return undefined;
}

function escapeHtml(value: string): string {
  return value
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#39;");
}

function buildMlbHeadshotUrl(mlbamid?: string | null): string {
  if (mlbamid && mlbamid.trim().length > 0 && mlbamid !== "0") {
    return `${MLB_PHOTO_BASE}/${encodeURIComponent(mlbamid)}/headshot/67/current`;
  }
  return MLB_GENERIC_HEADSHOT;
}

function toReadableVibe(mode: string): string {
  return mode
    .split("_")
    .map((segment) => segment.charAt(0).toUpperCase() + segment.slice(1))
    .join(" ");
}

function getAppStateParams(query: express.Request["query"]): URLSearchParams {
  const params = new URLSearchParams();
  const mode = getQueryValue(query.mode);
  const playerType = getQueryValue(query.playerType);
  const playerId = getQueryValue(query.playerId);
  const playerIds = getQueryValue(query.playerIds);
  const vibe = getQueryValue(query.vibe);

  if (mode === "compare" || mode === "single") {
    params.set("mode", mode);
  }
  if (playerType === "hitter" || playerType === "pitcher") {
    params.set("playerType", playerType);
  }
  if (playerId) {
    params.set("playerId", playerId);
  }
  if (playerIds) {
    params.set("playerIds", playerIds);
  }
  if (vibe) {
    params.set("vibe", vibe);
  }

  return params;
}

function getBaseUrl(req: express.Request): string {
  return `${req.protocol}://${req.get("host")}`;
}

function buildCardMetadata(req: express.Request): CardMetadata {
  const params = getAppStateParams(req.query);
  const playerType = (params.get("playerType") === "pitcher" ? "pitcher" : "hitter") as PlayerType;
  const mode = params.get("mode") === "compare" ? "compare" : "single";
  const rawVibe = params.get("vibe") ?? DEFAULT_ANALYSIS_MODE;
  const vibe = rawVibe in ANALYSIS_VIBES ? rawVibe : DEFAULT_ANALYSIS_MODE;
  const vibeLabel = toReadableVibe(vibe);

  const ids = (params.get("playerIds") ?? "")
    .split(",")
    .map((id) => id.trim())
    .filter(Boolean)
    .slice(0, 3);
  const singlePlayerId = params.get("playerId");
  const players = (mode === "compare" ? ids : singlePlayerId ? [singlePlayerId] : [])
    .map((id) => getPlayerById(playerType, id))
    .filter((entry): entry is NonNullable<typeof entry> => Boolean(entry));

  const canonicalPath = params.toString() ? `/?${params.toString()}` : "/";
  const canonicalUrl = `${getBaseUrl(req)}${canonicalPath}`;

  if (mode === "compare" && players.length > 1) {
    const names = players.map((player) => player.Name).filter(Boolean);
    const versus = names.join(" vs ");
    const title = `${versus} | ${APP_NAME} Comparison`;
    const description = `AI comparison in ${vibeLabel} mode for ${versus}. Stats, trends, and a clear recommendation.`;
    const imageUrl = buildMlbHeadshotUrl(players[0].mlbamid ?? null);
    return { title, description, imageUrl, canonicalUrl };
  }

  if (players.length === 1) {
    const player = players[0];
    const subject = playerType === "pitcher" ? "Pitching" : "Hitting";
    const title = `${player.Name} ${subject} Breakdown | ${APP_NAME}`;
    const description = `${subject} analysis for ${player.Name} in ${vibeLabel} mode. Data-backed takeaways in plain English.`;
    const imageUrl = buildMlbHeadshotUrl(player.mlbamid ?? null);
    return { title, description, imageUrl, canonicalUrl };
  }

  return {
    title: DEFAULT_TITLE,
    description: DEFAULT_DESCRIPTION,
    canonicalUrl,
  };
}

function buildSocialMetaTags(card: CardMetadata): string {
  const title = escapeHtml(card.title);
  const description = escapeHtml(card.description);
  const canonicalUrl = escapeHtml(card.canonicalUrl);
  const imageTags = card.imageUrl
    ? [
        `<meta property="og:image" content="${escapeHtml(card.imageUrl)}" />`,
        `<meta name="twitter:image" content="${escapeHtml(card.imageUrl)}" />`,
      ].join("\n    ")
    : "";
  const twitterCard = card.imageUrl ? "summary_large_image" : "summary";

  return [
    `<meta name="description" content="${description}" />`,
    `<meta property="og:type" content="website" />`,
    `<meta property="og:site_name" content="${APP_NAME}" />`,
    `<meta property="og:title" content="${title}" />`,
    `<meta property="og:description" content="${description}" />`,
    imageTags,
    `<meta property="og:url" content="${canonicalUrl}" />`,
    `<meta name="twitter:card" content="${twitterCard}" />`,
    `<meta name="twitter:title" content="${title}" />`,
    `<meta name="twitter:description" content="${description}" />`,
    `<link rel="canonical" href="${canonicalUrl}" />`,
  ]
    .filter(Boolean)
    .join("\n    ");
}

function injectMetaTags(indexHtml: string, metaTags: string): string {
  if (!indexHtml.includes("</head>")) {
    return indexHtml;
  }
  return indexHtml.replace("</head>", `    ${metaTags}\n  </head>`);
}

function buildSharePreviewHtml(card: CardMetadata, redirectTo: string): string {
  const title = escapeHtml(card.title);
  const description = escapeHtml(card.description);
  const canonicalUrl = escapeHtml(card.canonicalUrl);
  const safeRedirect = escapeHtml(redirectTo);
  const imageTags = card.imageUrl
    ? [
        `<meta property="og:image" content="${escapeHtml(card.imageUrl)}" />`,
        `<meta name="twitter:image" content="${escapeHtml(card.imageUrl)}" />`,
      ].join("\n    ")
    : "";
  const twitterCard = card.imageUrl ? "summary_large_image" : "summary";

  return `<!doctype html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>${title}</title>
    <meta name="description" content="${description}" />
    <meta property="og:type" content="website" />
    <meta property="og:site_name" content="${APP_NAME}" />
    <meta property="og:title" content="${title}" />
    <meta property="og:description" content="${description}" />
    ${imageTags}
    <meta property="og:url" content="${canonicalUrl}" />
    <meta name="twitter:card" content="${twitterCard}" />
    <meta name="twitter:title" content="${title}" />
    <meta name="twitter:description" content="${description}" />
    <meta http-equiv="refresh" content="0;url=${safeRedirect}" />
    <link rel="canonical" href="${canonicalUrl}" />
    <script>
      window.location.replace(${JSON.stringify(redirectTo)});
    </script>
  </head>
  <body>
    <p>Opening McFARLAND…</p>
  </body>
</html>`;
}

export function createServer() {
  const app = express();

  void initializeAnalytics();

  // Allow Express to respect proxy headers (Render/Heroku set X-Forwarded-For)
  const trustProxy = process.env.TRUST_PROXY ?? "1";
  app.set("trust proxy", trustProxy);

  const allowedOrigins = process.env.CLIENT_ORIGIN?.split(",").map((origin) => origin.trim()).filter(Boolean);
  const corsOptions = allowedOrigins && allowedOrigins.length > 0 ? { origin: allowedOrigins } : undefined;

  app.use(
    helmet({
      contentSecurityPolicy: {
        useDefaults: true,
        directives: {
          "img-src": ["'self'", "data:", "https://img.mlbstatic.com"],
          "script-src": ["'self'", "'unsafe-inline'", "https://cdn.amplitude.com"],
          "connect-src": ["'self'", "https://api2.amplitude.com", "https://api.amplitude.com"],
        },
      },
    })
  );
  app.use(cors(corsOptions));
  app.use(express.json({ limit: "1mb" }));
  app.use(adminModeMiddleware);

  app.use(router);

  const clientDistPath = path.resolve(currentDir, "..", "..", "client", "dist");
  if (fs.existsSync(clientDistPath)) {
    const indexHtmlPath = path.join(clientDistPath, "index.html");
    const indexTemplate = fs.readFileSync(indexHtmlPath, "utf8");

    app.get("/share", (req, res) => {
      const card = buildCardMetadata(req);
      const params = getAppStateParams(req.query);
      const target = params.toString() ? `/?${params.toString()}` : "/";
      res
        .setHeader("Content-Type", "text/html; charset=utf-8")
        .send(buildSharePreviewHtml(card, target));
    });

    app.use(express.static(clientDistPath, { index: false }));
    app.get("*", (req, res) => {
      const card = buildCardMetadata(req);
      const dynamicIndexHtml = injectMetaTags(indexTemplate, buildSocialMetaTags(card));
      res.setHeader("Content-Type", "text/html; charset=utf-8").send(dynamicIndexHtml);
    });
  }

  return app;
}

if (process.argv[1] === new URL(import.meta.url).pathname) {
  const port = Number(process.env.PORT ?? 3000);
  const app = createServer();
  app.listen(port, () => {
    console.log(`McFARLAND API listening on http://localhost:${port}`);
  });
}
