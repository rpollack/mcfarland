import express from "express";
import rateLimit from "express-rate-limit";
import {
  checkAdminPassword,
  clearAdminSessionCookie,
  isAdminLoginConfigured,
  setAdminSessionCookie,
} from "./admin.js";

const MAX_PASSWORD_LENGTH = 256;

// Only failed attempts count, so the owner logging in successfully never burns the budget.
const adminLoginLimiter = rateLimit({
  windowMs: 15 * 60_000,
  max: 5,
  skipSuccessfulRequests: true,
  standardHeaders: true,
  legacyHeaders: false,
});

const adminRouter = express.Router();

const urlencoded = express.urlencoded({ extended: false, limit: "2kb" });

function wantsJson(req: express.Request): boolean {
  return Boolean(req.headers.accept?.includes("application/json")) || req.is("application/json") === "application/json";
}

const ADMIN_PAGE_HTML = `<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <meta name="robots" content="noindex" />
    <title>Admin login</title>
  </head>
  <body style="font-family: system-ui, sans-serif; max-width: 22rem; margin: 4rem auto; padding: 0 1rem">
    <h1>Admin login</h1>
    <form method="post" action="/api/admin/login">
      <label>Password <input type="password" name="password" autocomplete="current-password" required maxlength="256" autofocus /></label>
      <button type="submit">Log in</button>
    </form>
  </body>
</html>`;

export function adminPageHandler(_req: express.Request, res: express.Response) {
  if (!isAdminLoginConfigured()) {
    return res.status(404).type("text/plain").send("Not found");
  }
  return res
    .status(200)
    .setHeader("Cache-Control", "no-store")
    .setHeader("X-Robots-Tag", "noindex")
    .type("text/html")
    .send(ADMIN_PAGE_HTML);
}

adminRouter.post("/api/admin/login", adminLoginLimiter, urlencoded, (req, res) => {
  if (!isAdminLoginConfigured()) {
    if (process.env.NODE_ENV === "production") {
      return res.status(503).json({ error: "Admin access is not configured" });
    }
    return res.status(404).json({ error: "Not found" });
  }

  const password: unknown = (req.body as { password?: unknown } | undefined)?.password;
  if (typeof password !== "string" || password.length === 0 || password.length > MAX_PASSWORD_LENGTH) {
    // Malformed attempts also count against the failed-login budget (any 4xx does).
    return res.status(400).json({ error: "A password is required" });
  }

  if (!checkAdminPassword(password)) {
    console.warn("[admin] failed login attempt", { ip: req.ip });
    return res.status(401).json({ error: "Incorrect password" });
  }

  setAdminSessionCookie(req, res);
  if (wantsJson(req)) {
    return res.status(204).end();
  }
  return res.redirect(303, "/");
});

adminRouter.post("/api/admin/logout", (req, res) => {
  clearAdminSessionCookie(req, res);
  res.status(204).end();
});

export default adminRouter;
