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
- Includes an admin-only social assistant that suggests timely player links and draft posts for X and Bluesky.
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
| `AMPLITUDE_API_KEY` | Sends analytics events to a specific Amplitude project. | built-in project key |
| `ADMIN_PASSWORD` | Enables admin mode protection for social assistant endpoints. | unset |
| `TRENDS_REFRESH_SECRET` | Required secret for `/api/trends/refresh`. | unset |
| `TRENDS_TIMEZONE` | Timezone used for trend date calculations. | `America/Chicago` |
| `CLIENT_ORIGIN` | Comma-separated allowed CORS origins. | unset |
| `TRUST_PROXY` | Express trust proxy setting for deployed environments. A number is the count of proxies in front of the app (e.g. `2` for Cloudflare plus Render); it determines the client IP used by rate limits. | `1` |
| `PORT` | API/server port. | `3000` |

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
