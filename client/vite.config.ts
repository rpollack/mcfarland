import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";

export default defineConfig({
  plugins: [react()],
  server: {
    port: 5173,
    // The client calls relative /api paths, so forward them to the API server that `npm run dev` starts.
    proxy: {
      "/api": "http://localhost:3000",
    },
  },
  preview: {
    port: 4173,
  },
});
