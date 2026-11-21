import crypto from "node:crypto";
import type { NextFunction, Request, Response } from "express";

const ADMIN_QUERY_PARAM = "admin";
const ADMIN_COOKIE_NAME = "mcfarland_admin_mode";
const ADMIN_COOKIE_MAX_AGE_MS = 12 * 60 * 60 * 1000; // 12 hours
const ADMIN_DISABLE_VALUES = new Set(["logout", "off", "false", "0"]);

export function getAdminPassword(): string | undefined {
  const raw = process.env.ADMIN_PASSWORD;
  if (!raw) {
    return undefined;
  }

  const password = raw.trim();
  return password.length > 0 ? password : undefined;
}

function safeEqual(a: string, b: string): boolean {
  const left = Buffer.from(a);
  const right = Buffer.from(b);
  if (left.length !== right.length) {
    return false;
  }
  return crypto.timingSafeEqual(left, right);
}

export function containsAdminSecret(value: string | null | undefined): boolean {
  const adminPassword = getAdminPassword();
  if (!adminPassword || !value) {
    return false;
  }

  const encoded = encodeURIComponent(adminPassword);
  if (value.includes(`admin=${adminPassword}`) || value.includes(`admin=${encoded}`)) {
    return true;
  }

  try {
    const decoded = decodeURIComponent(value);
    return decoded.includes(`admin=${adminPassword}`);
  } catch (_error) {
    return false;
  }
}

function parseCookies(header: string | undefined): Record<string, string> {
  if (!header) {
    return {};
  }

  return header
    .split(";")
    .map((segment) => segment.trim())
    .filter(Boolean)
    .reduce<Record<string, string>>((acc, segment) => {
      const [name, ...rest] = segment.split("=");
      if (!name) {
        return acc;
      }
      acc[name] = rest.join("=").trim();
      return acc;
    }, {});
}

function isSecureRequest(req: Request): boolean {
  if (req.secure) {
    return true;
  }
  const forwardedProto = req.headers["x-forwarded-proto"];
  if (typeof forwardedProto === "string") {
    return forwardedProto.split(",")[0]?.trim().toLowerCase() === "https";
  }
  return false;
}

function passwordFingerprint(password: string): string {
  return crypto.createHash("sha256").update(password).digest("hex").slice(0, 16);
}

/**
 * Key used to sign session cookies. Production requires ADMIN_SESSION_SECRET (fail closed); outside production the
 * password doubles as the key so local dev works without extra env.
 */
function getSessionSecret(): string | undefined {
  const secret = process.env.ADMIN_SESSION_SECRET?.trim();
  if (secret) {
    return secret;
  }
  return process.env.NODE_ENV === "production" ? undefined : getAdminPassword();
}

/** True when a password exists and sessions can be signed. */
export function isAdminLoginConfigured(): boolean {
  return Boolean(getAdminPassword() && getSessionSecret());
}

function sign(payload: string, secret: string): string {
  return crypto.createHmac("sha256", secret).update(payload).digest("base64url");
}

export function createAdminSessionToken(password: string, secret: string, now = Date.now()): string {
  const payload = Buffer.from(
    JSON.stringify({ exp: now + ADMIN_COOKIE_MAX_AGE_MS, pw: passwordFingerprint(password) })
  ).toString("base64url");
  return `${payload}.${sign(payload, secret)}`;
}

export function verifyAdminSessionToken(token: string | undefined, password: string, secret: string, now = Date.now()): boolean {
  if (!token) {
    return false;
  }
  const parts = token.split(".");
  if (parts.length !== 2 || !parts[0] || !parts[1]) {
    return false;
  }
  const [payload, signature] = parts;
  if (!safeEqual(signature, sign(payload, secret))) {
    return false;
  }
  try {
    const data = JSON.parse(Buffer.from(payload, "base64url").toString("utf8")) as { exp?: unknown; pw?: unknown };
    return (
      typeof data.exp === "number" &&
      data.exp > now &&
      typeof data.pw === "string" &&
      safeEqual(data.pw, passwordFingerprint(password))
    );
  } catch (_error) {
    return false;
  }
}

export function checkAdminPassword(candidate: unknown): boolean {
  const password = getAdminPassword();
  return password !== undefined && typeof candidate === "string" && safeEqual(candidate, password);
}

export function setAdminSessionCookie(req: Request, res: Response): void {
  const password = getAdminPassword();
  const secret = getSessionSecret();
  if (!password || !secret) {
    return;
  }
  res.cookie(ADMIN_COOKIE_NAME, createAdminSessionToken(password, secret), {
    httpOnly: true,
    sameSite: "lax",
    secure: isSecureRequest(req),
    maxAge: ADMIN_COOKIE_MAX_AGE_MS,
  });
}

export function clearAdminSessionCookie(req: Request, res: Response): void {
  res.clearCookie(ADMIN_COOKIE_NAME, {
    httpOnly: true,
    sameSite: "lax",
    secure: isSecureRequest(req),
  });
}

/**
 * Validates the signed session cookie and sets req.isAdminMode. The only query handling left is `?admin=logout|off|false|0`,
 * which clears the cookie; a password in the URL is never treated as a credential.
 */
export function adminModeMiddleware(req: Request, res: Response, next: NextFunction): void {
  const password = getAdminPassword();
  const secret = getSessionSecret();
  if (!password || !secret) {
    next();
    return;
  }

  const rawAdmin = (req.query as Record<string, unknown>)[ADMIN_QUERY_PARAM];
  const values = typeof rawAdmin === "string" ? [rawAdmin] : Array.isArray(rawAdmin) ? rawAdmin : [];
  if (values.some((value) => typeof value === "string" && ADMIN_DISABLE_VALUES.has(value.toLowerCase()))) {
    clearAdminSessionCookie(req, res);
    next();
    return;
  }

  const cookie = parseCookies(req.headers.cookie)[ADMIN_COOKIE_NAME];
  if (verifyAdminSessionToken(cookie, password, secret)) {
    req.isAdminMode = true;
  }
  next();
}

export function isAdminModeRequest(req: Request): boolean {
  return Boolean(req.isAdminMode);
}
