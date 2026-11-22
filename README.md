# McFARLAND

**Machine-crafted Forecasting And Reasoning for Luck, Analytics, Narratives, and Data**

McFARLAND is a baseball analysis app that turns current-season performance data into readable, shareable takes. It is built for the space between a spreadsheet and a group chat: enough statistical rigor to be useful, but written in plain English and customizable voices.

The app compares a player's current-year stats against his weighted baseline, highlights what has actually changed, and asks the question fans and fantasy players usually care about: should this get better, worse, or stay about the same?

## About The Project

McFARLAND exists because advanced baseball stats are useful, but they are often trapped in tables, acronyms, and context that casual fans do not want to decode every morning. The app takes the numbers that matter, filters out noise where it can, and turns them into short baseball arguments.

What it does:

- Builds single-player hitter and pitcher breakdowns using current-year stats, weighted baselines, skill indicators, luck indicators, and sample-size context. While a take is being written, the key stat signals show immediately with a loading skeleton.
- Compares two or three hitters or pitchers side by side and recommends the stronger profile. After a comparison, visitors can vote on who they would take and see how everyone else voted.
- Lets users choose an analysis "vibe" so the same data can sound straightforward, analytical, fantasy-focused, Gen Z, Shakespearean, sensationalist, optimistic, old-school, or cranky old-timer. Every vibe is a one-tap chip under a take, and "Surprise me" picks one at random.
- Shows a mirage board under the search box: the five hitters or pitchers whose season results ran furthest ahead of, and furthest behind, their expected stats (wOBA vs. xwOBA, ERA vs. xERA), with a short intro written in the reader's chosen vibe.
- Ends each single-player take with "Keep going" suggestions: two players with a similar luck gap (expected vs. actual results) and a natural rival to compare against.
- Provides a Fantasy Tools page with a daily start/sit recommendation for a player, using that day's MLB matchup (lineup status, probable pitchers, venue, and weather).
- Lets visitors star players. Stars are stored in the browser only.
- Surfaces quick-link player groups like breakouts, on-fire players, ice-cold players, and out-of-character profiles.
- Provides weekly trend views based on stored daily trend snapshots.
- Shows a dismissable "What's new" banner for announcements, with a history on the About page.
- Includes an admin-only social assistant that suggests timely player links and draft posts for X and Bluesky. Admins sign in at `/admin` with `ADMIN_PASSWORD`; the session is a signed, 12-hour cookie (a password in the URL is never accepted).
- Produces share-friendly URLs, dynamic Open Graph/Twitter metadata, and a generated 1200x630 preview image for player and comparison links.

The name is a tribute to former Orioles lefty T.J. McFarland, patron saint of adapting to the situation and doing useful work without needing the spotlight.

## Current Tech Stack

McFARLAND is a TypeScript monorepo with three npm workspaces:

- `client`: React 19, Vite, React Router, TanStack React Query, React Markdown, CSS modules, Vitest, Testing Library.
- `server`: Node.js, Express, TypeScript, Zod validation, Helmet, CORS, rate limiting, Anthropic Claude via `@anthropic-ai/sdk` (Messages API with structured outputs), PostgreSQL via `pg`, CSV parsing, share-image rendering with `satori` and `@resvg/resvg-js` (Inter from `@fontsource/inter`), Vitest, Supertest.
- `tests/e2e`: Playwright end-to-end tests.

The production server builds the React app and serves it from the Express API. Development runs Vite and the API side by side.

## Project Structure

```text
.
├── client/              # React + Vite UI
├── server/              # Express API and analysis services
├── tests/e2e/           # Playwright tests
├── full_stats_*.csv     # Current hitter/pitcher stat inputs
├── player_lookup.csv    # Player lookup metadata
├── data_freshness.json  # Current dataThroughDate metadata
├── refresh_data.R       # Data refresh script
└── README.md
```

## Core Runtime Behavior

- The server loads baseball CSV data at startup from the repository root.
- `/api/players`, `/api/players/:playerId`, `/api/trending`, and `/api/data-freshness` serve structured data to the client.
- `/api/players/:playerId/related` returns players with a similar luck gap and a rival to compare, computed from the current stats among players with a meaningful sample (300+ PA or batters faced). The luck-gap math lives in `server/src/luckGap.ts` and is shared with the mirage board.
- `/api/mirage-board` returns the board for hitters or pitchers (players with 300+ PA or batters faced, five per side). `/api/mirage-board/intro` writes its intro in the requested vibe through the same analysis cache as takes, so it costs one model call per vibe per data update, shared by everyone. If the intro cannot be written it returns a 502 and the board still renders without it.
- `/api/compare/votes` (GET and POST) records and tallies "who would you take" votes. One vote per browser, and the split is only returned to someone who has voted.
- `/api/announcement-events` logs views, clicks, and dismissals of the announcement banner.
- `/api/ui-events` logs mirage board views and row clicks (`mirage_board_viewed`, `mirage_board_player_clicked`) and player search (`player_search_performed` with the query and result count, `player_search_result_selected` with the rank chosen), vibe picks (`vibe_selected`, from a chip or Surprise me), votes (`compare_vote_cast`, with the matchup size and the position and name of the player chosen), star toggles (`player_star_toggled`, with whether the player was starred or unstarred), and "Keep going" clicks (`keep_going_clicked`, for a similar-luck-gap player or the rival compare). The body is a discriminated union on `event`, so new UI events need a schema entry in `server/src/routes.ts` and a type in `client/src/api.ts`.
- `/share` serves link-preview HTML, and `/share-image.png` renders the preview card (headline, players, vibe, and the stat behind the take). The renderer is loaded lazily and results are cached in memory.
- `/api/analyze` and `/api/compare/analyze` build prompts from the current stats and selected vibe, then use Claude (Claude Sonnet 5.5 by default) for headline + analysis generation, with JSON structured outputs and server-side refusal fallback.
- Successful analysis responses are cached in PostgreSQL by prompt hash, cache version, mode, and `dataThroughDate`, so repeat requests across users can skip the model call.
- Cache entries are valid only for the current `dataThroughDate`; stale rows are pruned opportunistically after successful writes.
- Analytics events are sent to Amplitude for sessions, analysis runs, share actions, mirage board views and clicks, player searches, vibe picks, votes, stars, "Keep going" clicks, and announcement views, clicks, and dismissals. Admin-mode requests are excluded from analytics.
- Every call to the model also records its cost: a `[model-usage]` server log line and a `model_call_completed` Amplitude event (sent under the constant user id `server`, so it is not tied to a visitor and is not excluded for admin requests). Each carries the feature (`single`, `compare`, `mirage_intro`, or `fantasy`), the analysis vibe, the requested and serving model, whether the refusal fallback ran, effort, stop reason, input, output (including thinking), and cache token counts, and duration. It never includes prompts, player names, or user data. Cache hits make no model call and log nothing, so cost per generated take can be separated from how often takes are generated.
- Compare votes are stored in the `compare_votes` table (one row per matchup and voter). The voter id is a random per-browser id kept separate from the analytics session id, sent in an `X-Voter-Id` header, and stored only as a SHA-256 hash. It is never sent to Amplitude; the `compare_vote_cast` event is sent under the analytics session id instead.
- Weekly trend storage uses PostgreSQL when `DATABASE_URL` is configured, with fallback behavior when storage is unavailable.

## Prerequisites

- Node.js 24 (what CI and the Docker image use)
- npm 9+
- Optional: Anthropic API key for generated analysis.
- Optional: PostgreSQL 17 or compatible Postgres database for shared analysis cache and weekly trend persistence.
- Optional: R runtime for running `refresh_data.R` locally.

## Getting Started

Install dependencies:

```bash
npm install
```

For local AI responses, set an Anthropic API key:

```bash
export ANTHROPIC_API_KEY="your-anthropic-key"
```

For Postgres-backed cache and trend storage, set:

```bash
export DATABASE_URL="postgres://..."
```

Start the development environment:

```bash
npm run dev
```

The client runs at `http://localhost:5173` and the API runs at `http://localhost:3000`.

Run the production build locally:

```bash
npm run start
```

The Express server serves the built React app at `http://localhost:3000`.

## Environment Variables

| Name | Purpose | Default |
| ---- | ------- | ------- |
| `ANTHROPIC_API_KEY` | Enables Claude-generated headline and analysis responses. | unset |
| `ANTHROPIC_MODEL` | Claude model used for generated headline and analysis responses. Non-Haiku models also get server-side refusal fallback. | `claude-sonnet-5-5` |
| `DATABASE_URL` | Enables Postgres-backed analysis cache and weekly trend storage. | unset |
| `AMPLITUDE_API_KEY` | Sends analytics events to a specific Amplitude project. Set it explicitly to report from a non-production run; set it empty to turn reporting off. | built-in project key, used only in live production (not tests, CI, local dev or Render preview deploys) |
| `ADMIN_PASSWORD` | Password entered at `/admin` to start an admin session. Failed logins are limited to 5 per IP per 15 minutes. Changing it signs out existing sessions. | unset |
| `ADMIN_SESSION_SECRET` | Random secret (32+ bytes, e.g. `openssl rand -base64 48`) used to sign admin session cookies. Required in production: without it admin login is disabled. Outside production it falls back to `ADMIN_PASSWORD`. | unset |
| `MODEL_MONTHLY_BUDGET_USD` | Turns on the model spend guard with this monthly budget in USD (see "Model spend guard"). | unset (guard off) |
| `MODEL_DAILY_CEILING_USD` | Daily spend ceiling in USD (UTC day). Must be above 0. | 10% of the monthly budget |
| `MODEL_IP_HOURLY_CAP` | New (non-cached) generations allowed per IP per hour; `0` turns the per-IP cap off. | `20` |
| `MODEL_CAP_ENFORCE` | `true` blocks generations over a limit with HTTP 429; anything else only logs what would have been blocked. | unset (log-only) |
| `TRENDS_REFRESH_SECRET` | Required secret for `/api/trends/refresh`. | unset |
| `TRENDS_TIMEZONE` | Timezone used for trend date calculations. | `America/Chicago` |
| `CLIENT_ORIGIN` | Comma-separated allowed CORS origins. | unset |
| `TRUST_PROXY` | Express trust proxy setting for deployed environments. A number is the count of proxies in front of the app (e.g. `2` for Cloudflare plus Render); it determines the client IP used by rate limits. | `1` |
| `PORT` | API/server port. | `3000` |

## Model spend guard

The guard keeps Claude spend inside a budget even if a bot or a bug hammers the site. It is off until `MODEL_MONTHLY_BUDGET_USD` is set.

- Cost is estimated from each call's token counts using the price table in `server/src/pricing.ts` (unknown models are priced at the most expensive known rates). Anthropic's console remains the source of truth for billing, so also set a spend limit on the Anthropic side.
- A new generation is blocked when this UTC month's spend has reached the monthly budget, today's UTC spend has reached the daily ceiling, or the caller's IP has made `MODEL_IP_HOURLY_CAP` new generations in the last hour. Days that are not used roll over automatically because only the month total and the daily ceiling are checked. A call already in flight can overshoot a limit by about one call's cost.
- Cached takes are never counted or blocked. Admin-mode requests are never blocked, but their spend counts. Calls made by the admin social assistant are not guarded, but their spend is recorded.
- Blocked requests get HTTP 429 with `{ "error": "...", "code": "spend_limit" }` and a `Retry-After` header; the app shows the message in place of the take.
- By default the guard is log-only: it records what it would have blocked and lets the request through. Set `MODEL_CAP_ENFORCE=true` to enforce.
- Spend is stored per day in the `model_spend_daily` Postgres table (needs `DATABASE_URL`). Without a database, or if it errors, the guard fails open and uses an in-process tally (with at most one warning an hour).
- Every limit hit (or would-be hit) is logged as a `[spend-guard] limit hit` line, stored in the `spend_limit_hits` table (feature, limit type, enforced or not, spend, and the anonymous session id, never an IP, prompt, or player), and sent to Amplitude as `spend_limit_hit`. The first request after UTC midnight logs `[spend-guard] daily summary` and sends `spend_daily_summary`.
- `GET /api/admin/spend` (admin mode) returns today's and this month's estimated cost and call counts, the budget, ceiling and enforcement mode, and the last seven days of limit hits.

## Announcements

Announcements are plain data in `client/src/announcements.ts` and ship with the deploy, so there is no extra request and nothing shifts after load. To announce something, add an entry to the top of the list:

```ts
{
  id: "2026-12-my-feature", // never reuse an id: dismissals are remembered by id
  lead: "New:",
  text: "One sentence about the feature.",
  link: { label: "See it", to: "/?mode=compare" }, // optional, in-app only
  startsAt: "2026-12-01", // UTC, inclusive
  endsAt: "2026-12-31",   // UTC, inclusive
},
```

Only one shows at a time: the most recently started one that is active and not dismissed. Dismissal is remembered per browser. Every announcement that has started also appears under "What's new" on the About page.

## Testing

Run server and client unit/API tests:

```bash
npm run test:unit
```

Run all workspace tests:

```bash
npm run test
```

Run Playwright e2e tests:

```bash
npm run test:e2e
```

The e2e command builds the app and starts the server itself, and it does not need an Anthropic key or a database. Without a key the AI responses fall back to an "unavailable" message, and votes use the in-memory store. If Playwright reports missing browsers, run `npx playwright install` first.

Run the server build only:

```bash
npm run build --workspace server
```

Run the full production build:

```bash
npm run build
```

## Data Pipeline

The app serves baseball data from CSV files in the repository root. They are produced by `refresh_data.R` and committed to git, so a data update is an ordinary PR.

**Inputs** (all in the repo root, committed):

- `fangraphs-leaderboards-<year>.csv` (hitters) and `pitcher-stats-<year>.csv` (pitchers): one file per baseline season. The three seasons before `current_year` in `refresh_data.R` form each player's weighted baseline (weights 1, 3, 5, oldest first). Rolling into a new year is described at the top of the script; `Rscript refresh_data.R export-pitcher-season <year>` creates the pitcher file.
- Live current-season pulls on every run: the MLB Stats API, FanGraphs (via `baseballr` for hitters and the FanGraphs leaders API for pitchers).

**Outputs committed to git**: `full_stats_hitters.csv`, `full_stats_pitchers.csv`, `player_lookup.csv`, and `data_freshness.json` (when the data is through, and when it was refreshed).

**Scratch files**: the script also writes `candidate_full_stats_*.csv`, `candidate_player_lookup.csv`, `served_player_inventory.csv` and `migration_parity_report.json`. They are gitignored and regenerated on every run. The candidates are checked against the current files (row counts, player IDs, MLB IDs, stat parity) and only copied over the real CSVs if the check passes; otherwise the production files are left alone and the failures are printed.

**Daily schedule** (`.github/workflows/refresh-data.yml`, 10:37 UTC, also runnable manually; set `publish` to false for a dry run):

1. Installs R and packages, then runs `Rscript refresh_data.R`.
2. "Skip freshness-only updates": if the three stat CSVs did not change, `data_freshness.json` is restored so no PR is opened (no new games means no PR, and cached takes are not regenerated).
3. Otherwise opens a `Daily baseball data refresh YYYY-MM-DD` PR and enables squash auto-merge. This needs the `REFRESH_PR_TOKEN` secret, and the PR must pass the `playwright` check before it merges (master is protected).
4. Waits for the merge, then waits until the deployed app reports the new `dataThroughDate`.
5. Calls `generate-trend-snapshots.yml`, which POSTs to `TRENDS_SNAPSHOT_URL` with the `TRENDS_REFRESH_SECRET` secret so the server records the weekly trend snapshot from the new data. (It runs from the refresh workflow so it cannot snapshot yesterday's data.)
6. A monitor job fails with a clear message after repeated failures or stale live data during the season.

**Deploys**: Render redeploys on every merge to master. The Docker image contains the repo, and the server reads the CSVs and `data_freshness.json` from the repo root (`DATA_ROOT` in `server/src/dataStore.ts`), so merging the refresh PR is what publishes new data.

**Run it locally**: `Rscript refresh_data.R` from the repo root (needs R with `dplyr`, `readr`, `tibble`, `stringr`, `stringi`, `jsonlite`, `janitor`, `httr`, `rvest`, `baseballr`). It overwrites the committed CSVs when the check passes; review with `git diff` and discard with `git checkout` if you do not want to keep it.

Do not move the CSVs without updating `DATA_ROOT`, the `Dockerfile`, `refresh_data.R`, and the workflows.

## Data And Caching Notes

- The app's baseball data comes from the CSV files in the repository root plus `data_freshness.json`.
- `refresh_data.R` is the data refresh entrypoint and is intended to be run on a schedule during the season.
- The daily refresh workflow only opens a PR when the stat files (`full_stats_*.csv`, `player_lookup.csv`) actually change. In the offseason nothing changes, so the data and `dataThroughDate` stay frozen and nothing is regenerated.
- Analysis cache validity follows `dataThroughDate`, not wall-clock date.
- Only successful structured Claude responses are cached. API errors, unavailable responses, and malformed model responses are not cached.
- The analysis cache is shared across users and sessions when Postgres is configured.
- Without `DATABASE_URL`, analysis still works, but shared cache and persisted weekly trend storage are disabled. Compare votes fall back to a bounded in-memory store that resets on restart.

## License

Educational and personal use only. Respect applicable baseball data and image terms.
