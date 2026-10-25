import request from "supertest";
import { afterEach, describe, expect, it } from "vitest";
import { createServer } from "../src/index.js";

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
