import request from "supertest";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { createServer } from "../src/index.js";
import { createAdminSessionToken, verifyAdminSessionToken } from "../src/admin.js";

// Login limiters are module-level, so this file has its own budget; each test uses its own forwarded IP.
const app = createServer();
const COOKIE = "mcfarland_admin_mode";
let ipCounter = 0;
const nextIp = () => `203.0.113.${(ipCounter += 1)}`;

function cookieFrom(res: { headers: Record<string, unknown> }): string | undefined {
  const setCookie = ([] as string[]).concat((res.headers["set-cookie"] as string[] | string | undefined) ?? []);
  return setCookie.find((c) => c.startsWith(`${COOKIE}=`));
}

function login(password: unknown, ip = nextIp()) {
  return request(app).post("/api/admin/login").set("X-Forwarded-For", ip).set("Accept", "application/json").send({ password });
}

const suggestions = (cookie: string) => request(app).get("/api/admin/social-suggestions").set("Cookie", cookie);

describe("admin session tokens", () => {
  it("round-trips and rejects tampering, expiry, wrong secret and rotated password", () => {
    const now = 1_700_000_000_000;
    const token = createAdminSessionToken("top-secret", "s3cret", now);
    expect(verifyAdminSessionToken(token, "top-secret", "s3cret", now + 1000)).toBe(true);
    expect(verifyAdminSessionToken(token, "top-secret", "s3cret", now + 12 * 3600_000 + 1)).toBe(false);
    expect(verifyAdminSessionToken(token, "top-secret", "other", now)).toBe(false);
    expect(verifyAdminSessionToken(token, "rotated", "s3cret", now)).toBe(false);
    const [payload, sig] = token.split(".");
    const forgedPayload = Buffer.from(JSON.stringify({ exp: now + 10 ** 12, pw: "x" })).toString("base64url");
    expect(verifyAdminSessionToken(`${forgedPayload}.${sig}`, "top-secret", "s3cret", now)).toBe(false);
    expect(verifyAdminSessionToken(`${payload}.${sig}x`, "top-secret", "s3cret", now)).toBe(false);
    expect(verifyAdminSessionToken("1", "top-secret", "s3cret", now)).toBe(false);
    expect(verifyAdminSessionToken(undefined, "top-secret", "s3cret", now)).toBe(false);
  });
});

describe("admin login", () => {
  beforeEach(() => {
    process.env.ADMIN_PASSWORD = "top-secret";
    vi.spyOn(console, "warn").mockImplementation(() => undefined);
    // The production-mode tests reach the social suggestions route, which skips its Google News lookups only when
    // NODE_ENV is "test". Fail every outgoing fetch right away so these tests never wait on the network.
    vi.stubGlobal("fetch", vi.fn().mockRejectedValue(new Error("network disabled in tests")));
  });

  afterEach(() => {
    delete process.env.ADMIN_PASSWORD;
    delete process.env.ADMIN_SESSION_SECRET;
    vi.unstubAllEnvs();
    vi.unstubAllGlobals();
    vi.useRealTimers();
    vi.restoreAllMocks();
  });

  it("logs in with the right password and sets an httpOnly cookie that works", async () => {
    const res = await login("top-secret");
    expect(res.status).toBe(204);
    const cookie = cookieFrom(res)!;
    expect(cookie).toContain("HttpOnly");
    expect(cookie).not.toContain("top-secret");
    expect(cookie.split(";")[0].split("=")[1]).not.toBe("1");
    expect((await suggestions(cookie.split(";")[0])).status).toBe(200);
  });

  it("rejects a wrong password without a cookie and never logs the password", async () => {
    const res = await login("nope");
    expect(res.status).toBe(401);
    expect(cookieFrom(res)).toBeUndefined();
    expect(JSON.stringify(vi.mocked(console.warn).mock.calls)).not.toContain("nope");
  });

  it("rejects missing, non-string and oversized passwords with 400", async () => {
    expect((await login(undefined)).status).toBe(400);
    expect((await login(12345)).status).toBe(400);
    expect((await login("x".repeat(5000))).status).toBe(400);
  });

  it("rejects forged, old-format, expired and tampered cookies", async () => {
    const good = cookieFrom(await login("top-secret"))!.split(";")[0].split("=")[1];
    const [payload, sig] = good.split(".");
    const forgedPayload = Buffer.from(JSON.stringify({ exp: Date.now() + 10 ** 10, pw: "x" })).toString("base64url");
    for (const bad of ["1", "deadbeef", `${forgedPayload}.${sig}`, `${payload}.AAAA`]) {
      expect((await suggestions(`${COOKIE}=${bad}`)).status).toBe(403);
    }
    vi.useFakeTimers({ toFake: ["Date"] });
    vi.setSystemTime(Date.now() + 12 * 3600_000 + 60_000);
    expect((await suggestions(`${COOKIE}=${good}`)).status).toBe(403);
  });

  it("invalidates sessions when the password is rotated", async () => {
    const cookie = cookieFrom(await login("top-secret"))!.split(";")[0];
    process.env.ADMIN_PASSWORD = "rotated-secret";
    expect((await suggestions(cookie)).status).toBe(403);
  });

  it("uses ADMIN_SESSION_SECRET for signing when set", async () => {
    process.env.ADMIN_SESSION_SECRET = "signing-key-one";
    const cookie = cookieFrom(await login("top-secret"))!.split(";")[0];
    expect((await suggestions(cookie)).status).toBe(200);
    process.env.ADMIN_SESSION_SECRET = "signing-key-two";
    expect((await suggestions(cookie)).status).toBe(403);
  });

  it("fails closed in production without ADMIN_SESSION_SECRET", async () => {
    vi.stubEnv("NODE_ENV", "production");
    const res = await login("top-secret");
    expect(res.status).toBe(503);
    expect(cookieFrom(res)).toBeUndefined();
    expect((await request(app).get("/api/admin/social-suggestions")).status).toBe(503);
    expect((await request(app).get("/admin")).status).toBe(404);
  });

  it("works in production when ADMIN_SESSION_SECRET is set", async () => {
    vi.stubEnv("NODE_ENV", "production");
    process.env.ADMIN_SESSION_SECRET = "signing-key-one";
    const res = await login("top-secret");
    expect(res.status).toBe(204);
    expect((await suggestions(cookieFrom(res)!.split(";")[0])).status).toBe(200);
  });

  it("returns 404 for login in dev when no password is configured", async () => {
    delete process.env.ADMIN_PASSWORD;
    expect((await login("anything")).status).toBe(404);
  });

  it("does not treat ?admin=<password> as a credential on any route", async () => {
    const res = await request(app).get("/api/admin/social-suggestions?admin=top-secret");
    expect(res.status).toBe(403);
    expect(cookieFrom(res)).toBeUndefined();
  });

  it("clears the cookie via ?admin=logout and POST /api/admin/logout", async () => {
    const cookie = cookieFrom(await login("top-secret"))!.split(";")[0];
    const viaQuery = await request(app).get("/api/admin/social-suggestions?admin=logout").set("Cookie", cookie);
    expect(viaQuery.status).toBe(403);
    expect(cookieFrom(viaQuery)).toMatch(new RegExp(`^${COOKIE}=;`));
    const viaPost = await request(app).post("/api/admin/logout").set("Cookie", cookie);
    expect(viaPost.status).toBe(204);
    expect(cookieFrom(viaPost)).toMatch(new RegExp(`^${COOKIE}=;`));
  });

  it("serves the login page with no-store and noindex, and 303s a form login to /", async () => {
    const page = await request(app).get("/admin");
    expect(page.status).toBe(200);
    expect(page.headers["cache-control"]).toBe("no-store");
    expect(page.text).toContain('name="robots" content="noindex"');
    expect(page.text).toContain('type="password"');

    const form = await request(app)
      .post("/api/admin/login")
      .set("X-Forwarded-For", nextIp())
      .type("form")
      .send({ password: "top-secret" });
    expect(form.status).toBe(303);
    expect(form.headers.location).toBe("/");
    expect(cookieFrom(form)).toBeDefined();
  });

  it("404s the login page when admin is not configured", async () => {
    delete process.env.ADMIN_PASSWORD;
    expect((await request(app).get("/admin")).status).toBe(404);
  });

  it("limits to 5 failed attempts per IP, and successful logins do not use the budget", async () => {
    const ip = nextIp();
    for (let i = 0; i < 3; i += 1) {
      expect((await login("top-secret", ip)).status).toBe(204);
    }
    for (let i = 0; i < 5; i += 1) {
      expect((await login("wrong", ip)).status).toBe(401);
    }
    expect((await login("wrong", ip)).status).toBe(429);
    expect((await login("top-secret", ip)).status).toBe(429);
    // A different client is unaffected.
    expect((await login("top-secret")).status).toBe(204);
  });
});
