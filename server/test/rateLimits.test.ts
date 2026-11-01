import request from "supertest";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { createServer } from "../src/index.js";

// Limiters are module-level, so this file gets its own copy (and request budget) separate from routes.test.ts.
const app = createServer();

async function exhaust(send: () => Promise<{ status: number }>, allowed: number) {
  for (let i = 0; i < allowed; i += 1) {
    expect((await send()).status).not.toBe(429);
  }
  return send();
}

describe("rate limits on public endpoints", () => {
  beforeEach(() => {
    // Session and share events forward to Amplitude; never let tests reach the real service.
    vi.spyOn(globalThis, "fetch").mockResolvedValue({ ok: true } as Response);
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("limits /api/trending to 60 requests per minute", async () => {
    const blocked = await exhaust(() => request(app).get("/api/trending"), 60);

    expect(blocked.status).toBe(429);
  });

  it("limits session and share events to 60 requests per minute combined", async () => {
    const shareEvent = {
      sessionId: "s",
      playerName: "Aaron Judge",
      analysisMode: "gen_z",
      eventType: "copy",
    };
    for (let i = 0; i < 30; i += 1) {
      expect((await request(app).post("/api/sessions").send({ sessionId: "s" })).status).toBe(204);
    }
    for (let i = 0; i < 30; i += 1) {
      expect((await request(app).post("/api/share-events").send(shareEvent)).status).not.toBe(429);
    }

    const blocked = await request(app).post("/api/sessions").send({ sessionId: "s" });

    expect(blocked.status).toBe(429);
  });

  describe("client IP detection behind proxies", () => {
    afterEach(() => {
      vi.unstubAllEnvs();
    });

    async function remaining(app: ReturnType<typeof createServer>, forwardedFor: string) {
      const response = await request(app).get("/api/trending").set("X-Forwarded-For", forwardedFor);
      return Number(response.headers["ratelimit-remaining"]);
    }

    it("treats TRUST_PROXY=2 as two proxy hops, so each real client gets its own budget", async () => {
      vi.stubEnv("TRUST_PROXY", "2");
      const app = createServer();

      // X-Forwarded-For is "<client>, <edge proxy>": the edge address is shared by everyone.
      const first = await remaining(app, "203.0.113.1, 198.51.100.7");
      const second = await remaining(app, "203.0.113.1, 198.51.100.7");
      const otherClient = await remaining(app, "203.0.113.2, 198.51.100.7");

      expect(second).toBe(first - 1);
      expect(otherClient).toBe(first);
    });

    it("defaults to one proxy hop", async () => {
      vi.stubEnv("TRUST_PROXY", "");
      const app = createServer();

      const first = await remaining(app, "203.0.113.3, 198.51.100.8");
      const second = await remaining(app, "203.0.113.4, 198.51.100.8");

      expect(second).toBe(first - 1);
    });
  });
});
