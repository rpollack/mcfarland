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
});
