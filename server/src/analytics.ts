import { containsAdminSecret } from "./admin.js";

const DEFAULT_AMPLITUDE_API_KEY = "f7a70fb74292a0b7b9f5dced71f2931";
const AMPLITUDE_HTTP_API = "https://api2.amplitude.com/2/httpapi";

function isTruthyEnv(value: string | undefined): boolean {
  return value !== undefined && value !== "" && value !== "0" && value.toLowerCase() !== "false";
}

/**
 * Which Amplitude key to send with, or null to send nothing.
 *
 * Only the live production service reports to the built-in project. Unit tests, CI (including the Playwright
 * e2e run), local development and Render preview deploys must never create analytics events, so they get no
 * key. Setting AMPLITUDE_API_KEY explicitly is the one deliberate opt-in (an empty value turns reporting off).
 */
export function resolveAmplitudeApiKey(env: NodeJS.ProcessEnv = process.env): string | null {
  const explicit = env.AMPLITUDE_API_KEY;
  if (explicit !== undefined) {
    return explicit.trim() || null;
  }
  const isLiveProduction =
    env.NODE_ENV === "production" &&
    !isTruthyEnv(env.CI) &&
    !isTruthyEnv(env.VITEST) &&
    !isTruthyEnv(env.IS_PULL_REQUEST); // Render sets this on pull request preview deploys.
  return isLiveProduction ? DEFAULT_AMPLITUDE_API_KEY : null;
}

function shouldSkipLogging(...sources: Array<string | null | undefined>): boolean {
  return sources.some((source) => containsAdminSecret(source));
}

function truncate(value: string | null | undefined, maxLength: number): string | null {
  if (!value) {
    return null;
  }
  if (value.length <= maxLength) {
    return value;
  }
  return value.slice(0, maxLength);
}

type AmplitudeEvent = {
  event_type: string;
  user_id: string;
  time?: number;
  event_properties?: Record<string, unknown>;
};

async function sendAmplitudeEvent(event: AmplitudeEvent): Promise<void> {
  const apiKey = resolveAmplitudeApiKey();
  if (!apiKey) {
    return;
  }

  try {
    const response = await fetch(AMPLITUDE_HTTP_API, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        api_key: apiKey,
        events: [event],
      }),
      signal: AbortSignal.timeout(5000),
    });

    if (!response.ok) {
      const body = await response.text().catch(() => "");
      console.warn("[analytics] failed to forward event to Amplitude", {
        eventType: event.event_type,
        status: response.status,
        body: body.slice(0, 500),
      });
    }
  } catch (error) {
    console.warn("[analytics] failed to reach Amplitude", {
      eventType: event.event_type,
      error,
    });
  }
}

export async function initializeAnalytics(): Promise<void> {
  // Analytics logging is Amplitude-only. SQL-backed initialization was removed.
}

type AnalysisLogEvent = {
  sessionId: string;
  playerId?: string | null;
  playerName: string;
  analysisMode: string;
  playerType: "hitter" | "pitcher";
  eventType: "single" | "compare" | "fantasy";
  referer?: string | null;
};

type ShareLogEvent = {
  sessionId: string;
  playerName: string;
  analysisMode: string;
  eventType: string;
  playerType?: "hitter" | "pitcher";
  shareUrl?: string | null;
  referer?: string | null;
};

export async function logSessionStart(sessionId: string, referer?: string | null): Promise<void> {
  if (!sessionId) {
    return;
  }
  if (shouldSkipLogging(referer)) {
    return;
  }

  await sendAmplitudeEvent({
    event_type: "session_started",
    user_id: sessionId,
    time: Date.now(),
    event_properties: {
      referer: truncate(referer, 512),
    },
  });
}

export async function logAnalysisEvent(event: AnalysisLogEvent): Promise<void> {
  if (!event.sessionId) {
    return;
  }
  if (shouldSkipLogging(event.referer)) {
    return;
  }

  await sendAmplitudeEvent({
    event_type: "analysis_logged",
    user_id: event.sessionId,
    time: Date.now(),
    event_properties: {
      player_id: truncate(event.playerId ?? null, 128),
      player_name: truncate(event.playerName, 256),
      analysis_mode: truncate(event.analysisMode, 64),
      mode: event.eventType,
      player_type: event.playerType,
      event_type: event.eventType,
      referer: truncate(event.referer, 512),
    },
  });
}

export async function logShareEvent(event: ShareLogEvent): Promise<void> {
  if (!event.sessionId) {
    return;
  }
  if (shouldSkipLogging(event.referer, event.shareUrl)) {
    return;
  }

  await sendAmplitudeEvent({
    event_type: "share_event_logged",
    user_id: event.sessionId,
    time: Date.now(),
    event_properties: {
      player_name: truncate(event.playerName, 256),
      analysis_mode: truncate(event.analysisMode, 64),
      event_type: truncate(event.eventType, 64),
      player_type: event.playerType ?? null,
      share_url: truncate(event.shareUrl ?? null, 1024),
      referer: truncate(event.referer, 512),
    },
  });
}

type AnnouncementLogEvent = {
  sessionId: string;
  announcementId: string;
  action: "viewed" | "clicked" | "dismissed";
  referer?: string | null;
};

export async function logAnnouncementEvent(event: AnnouncementLogEvent): Promise<void> {
  if (!event.sessionId) {
    return;
  }
  if (shouldSkipLogging(event.referer)) {
    return;
  }

  await sendAmplitudeEvent({
    event_type: "announcement_event_logged",
    user_id: event.sessionId,
    time: Date.now(),
    event_properties: {
      announcement_id: truncate(event.announcementId, 128),
      action: event.action,
      referer: truncate(event.referer, 512),
    },
  });
}

type UiLogEvent = {
  sessionId: string;
  eventType: string;
  properties: Record<string, string | number | boolean | null | undefined>;
  referer?: string | null;
};

function toSnakeCase(key: string): string {
  return key.replace(/[A-Z]/g, (letter) => `_${letter.toLowerCase()}`);
}

export async function logUiEvent(event: UiLogEvent): Promise<void> {
  if (!event.sessionId) {
    return;
  }
  const stringProperties = Object.values(event.properties).filter(
    (value): value is string => typeof value === "string"
  );
  if (shouldSkipLogging(event.referer, ...stringProperties)) {
    return;
  }

  const eventProperties: Record<string, unknown> = {};
  for (const [key, value] of Object.entries(event.properties)) {
    eventProperties[toSnakeCase(key)] = typeof value === "string" ? truncate(value, 256) : (value ?? null);
  }

  await sendAmplitudeEvent({
    event_type: event.eventType,
    user_id: event.sessionId,
    time: Date.now(),
    event_properties: {
      ...eventProperties,
      referer: truncate(event.referer, 512),
    },
  });
}

/**
 * Cost telemetry for one model call. These are server-side events with no session, player or user data,
 * so they use a constant user id and are not subject to admin-mode exclusion.
 */
export async function logModelCall(record: Record<string, string | number | boolean | null>): Promise<void> {
  const eventProperties: Record<string, unknown> = {};
  for (const [key, value] of Object.entries(record)) {
    eventProperties[toSnakeCase(key)] = value;
  }

  await sendAmplitudeEvent({
    event_type: "model_call_completed",
    user_id: "server",
    time: Date.now(),
    event_properties: eventProperties,
  });
}

/** A generation request hit (or, in log-only mode, would have hit) a spend limit. Never carries IP, prompts or player names. */
export async function logSpendLimitHit(
  record: Record<string, string | number | boolean | null>,
  sessionId: string | null
): Promise<void> {
  const eventProperties: Record<string, unknown> = {};
  for (const [key, value] of Object.entries(record)) {
    eventProperties[toSnakeCase(key)] = value;
  }

  await sendAmplitudeEvent({
    event_type: "spend_limit_hit",
    user_id: sessionId || "server",
    time: Date.now(),
    event_properties: eventProperties,
  });
}

/** One summary per UTC day of model spend and limit hits. */
export async function logSpendDailySummary(record: Record<string, string | number | boolean | null>): Promise<void> {
  const eventProperties: Record<string, unknown> = {};
  for (const [key, value] of Object.entries(record)) {
    eventProperties[toSnakeCase(key)] = value;
  }

  await sendAmplitudeEvent({
    event_type: "spend_daily_summary",
    user_id: "server",
    time: Date.now(),
    event_properties: eventProperties,
  });
}
