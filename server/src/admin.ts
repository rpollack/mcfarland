import type { NextFunction, Request, Response } from "express";

const ADMIN_QUERY_PARAM = "admin";
const ADMIN_COOKIE_NAME = "mcfarland_admin_mode";
const ADMIN_COOKIE_VALUE = "1";
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

function extractQueryValues(value: unknown): string[] {
  if (typeof value === "string") {
    return [value];
  }
  if (Array.isArray(value)) {
    return value.filter((entry): entry is string => typeof entry === "string");
  }
  return [];
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

export function adminModeMiddleware(req: Request, res: Response, next: NextFunction): void {
  const adminPassword = getAdminPassword();
  if (!adminPassword) {
    next();
    return;
  }

  const existingCookies = parseCookies(req.headers.cookie);
  let isAdminMode = existingCookies[ADMIN_COOKIE_NAME] === ADMIN_COOKIE_VALUE;
  let shouldClearCookie = false;

  if (!isAdminMode) {
    const query = req.query as Record<string, unknown>;
    const queryValues = extractQueryValues(query[ADMIN_QUERY_PARAM]);
    if (queryValues.length > 0) {
      const normalized = queryValues.map((value) => value?.toString().toLowerCase());
      if (normalized.some((value) => ADMIN_DISABLE_VALUES.has(value ?? ""))) {
        shouldClearCookie = Boolean(existingCookies[ADMIN_COOKIE_NAME]);
        isAdminMode = false;
      } else {
        isAdminMode = queryValues.some((value) => value === adminPassword);
      }
    }
  } else {
    // Allow disabling admin mode even if cookie already set
    const query = req.query as Record<string, unknown>;
    const queryValues = extractQueryValues(query[ADMIN_QUERY_PARAM]);
    if (queryValues.length > 0) {
      const normalized = queryValues.map((value) => value?.toString().toLowerCase());
      if (normalized.some((value) => ADMIN_DISABLE_VALUES.has(value ?? ""))) {
        shouldClearCookie = true;
        isAdminMode = false;
      }
    }
  }

  if (isAdminMode) {
    req.isAdminMode = true;
    if (existingCookies[ADMIN_COOKIE_NAME] !== ADMIN_COOKIE_VALUE) {
      res.cookie(ADMIN_COOKIE_NAME, ADMIN_COOKIE_VALUE, {
        httpOnly: true,
        sameSite: "lax",
        secure: isSecureRequest(req),
        maxAge: ADMIN_COOKIE_MAX_AGE_MS,
      });
    }
  } else if (shouldClearCookie) {
    res.clearCookie(ADMIN_COOKIE_NAME, {
      httpOnly: true,
      sameSite: "lax",
      secure: isSecureRequest(req),
    });
  }

  next();
}

export function isAdminModeRequest(req: Request): boolean {
  return Boolean(req.isAdminMode);
}
