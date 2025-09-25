import "dotenv/config";
import path from "node:path";
import { fileURLToPath } from "node:url";
import express from "express";
import helmet from "helmet";
import cors from "cors";
import router from "./routes.js";

const currentDir = path.dirname(fileURLToPath(import.meta.url));

export function createServer() {
  const app = express();

  const allowedOrigins = process.env.CLIENT_ORIGIN?.split(",").map((origin) => origin.trim()).filter(Boolean);
  const corsOptions = allowedOrigins && allowedOrigins.length > 0 ? { origin: allowedOrigins } : undefined;

  app.use(helmet());
  app.use(cors(corsOptions));
  app.use(express.json({ limit: "1mb" }));

  app.use(router);

  if (process.env.NODE_ENV === "production") {
    const clientDistPath = path.resolve(currentDir, "..", "..", "client", "dist");
    app.use(express.static(clientDistPath));
    app.get("*", (_req, res) => {
      res.sendFile(path.join(clientDistPath, "index.html"));
    });
  }

  return app;
}

if (process.argv[1] === new URL(import.meta.url).pathname) {
  const port = Number(process.env.PORT ?? 3000);
  const app = createServer();
  app.listen(port, () => {
    console.log(`McFarland API listening on http://localhost:${port}`);
  });
}
