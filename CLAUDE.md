@AGENTS.md

# Claude Code notes

- GitHub repo: `rpollack/mcfarland` (public). `master` is protected: changes land via PR, and the `playwright` check must pass.
- Never commit secrets or `.env` files; the repo is public.
- The `refresh-data.yml` workflow opens a `Daily baseball data refresh YYYY-MM-DD` PR from an `automated/daily-data-refresh-*` branch every day and auto-merges it, so `master` moves daily; always branch from a freshly fast-forwarded `master`.

## Commands

- Install: `npm ci`
- Dev (API + Vite): `npm run dev`
- Unit tests (server + client): `npm run test:unit`
- E2E tests (Playwright): `npm run test:e2e`
- Build: `npm run build`
- Lint: `npm run lint`
