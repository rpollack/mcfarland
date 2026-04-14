import { containsAdminSecret } from "./admin.js";

const DEFAULT_AMPLITUDE_API_KEY = "f7a70fb74292a0b7b9f5dced71f2931";
const AMPLITUDE_HTTP_API = "https://api2.amplitude.com/2/httpapi";

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
  const apiKey = (process.env.AMPLITUDE_API_KEY ?? DEFAULT_AMPLITUDE_API_KEY).trim();
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
  playerName: string;
  analysisMode: string;
  playerType: "hitter" | "pitcher";
  eventType: "single" | "compare";
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
      player_name: truncate(event.playerName, 256),
      analysis_mode: truncate(event.analysisMode, 64),
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
