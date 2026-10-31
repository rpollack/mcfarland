import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import request from "supertest";
import { afterEach, describe, expect, it } from "vitest";
import { createServer } from "../src/index.js";

const clientDistPath = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..", "..", "client", "dist");
const hasClientBuild = fs.existsSync(path.join(clientDistPath, "index.html"));

describe("security headers", () => {
  const originalNodeEnv = process.env.NODE_ENV;

  afterEach(() => {
    process.env.NODE_ENV = originalNodeEnv;
  });

  it("omits HTTPS-only headers outside production so Safari can load http://localhost", async () => {
    process.env.NODE_ENV = "development";
    const response = await request(createServer()).get("/health");

    expect(response.headers["content-security-policy"]).not.toContain("upgrade-insecure-requests");
    expect(response.headers["strict-transport-security"]).toBeUndefined();
  });

  it("keeps HTTPS-only headers in production", async () => {
    process.env.NODE_ENV = "production";
    const response = await request(createServer()).get("/health");

    expect(response.headers["content-security-policy"]).toContain("upgrade-insecure-requests");
    expect(response.headers["strict-transport-security"]).toContain("max-age=");
  });
});

describe.skipIf(!hasClientBuild)("client delivery cache headers", () => {
  it("makes HTML responses revalidate so deploys take effect immediately", async () => {
    const app = createServer();

    for (const url of ["/", "/share?mode=single", "/some/client/route", "/index.html"]) {
      const response = await request(app).get(url);
      expect(response.status).toBe(200);
      expect(response.headers["content-type"]).toContain("text/html");
      expect(response.headers["cache-control"]).toBe("no-cache");
    }
  });

  it("covers hashed CSS and JS but nothing else with immutable caching", async () => {
    const app = createServer();
    const names = fs.readdirSync(path.join(clientDistPath, "assets"));

    expect(names.some((name) => name.endsWith(".css"))).toBe(true);
    for (const name of names) {
      const response = await request(app).get(`/assets/${name}`);
      expect(response.headers["cache-control"]).toContain("immutable");
    }
    const indexHtml = await request(app).get("/index.html");
    expect(indexHtml.headers["cache-control"]).not.toContain("immutable");
  });

  it("marks content-hashed assets as immutable", async () => {
    const asset = fs.readdirSync(path.join(clientDistPath, "assets")).find((name) => name.endsWith(".js"));
    expect(asset).toBeDefined();

    const response = await request(createServer()).get(`/assets/${asset}`);

    expect(response.status).toBe(200);
    expect(response.headers["cache-control"]).toBe("public, max-age=31536000, immutable");
  });

  it("does not mark missing assets as cacheable", async () => {
    const response = await request(createServer()).get("/assets/missing-abc123.js");

    expect(response.status).toBe(404);
    expect(response.headers["cache-control"] ?? "").not.toContain("immutable");
  });
});
