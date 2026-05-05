# McFARLAND

**Machine-crafted Forecasting And Reasoning for Luck, Analytics, Narratives, and Data**

McFARLAND is a baseball analysis app that turns current-season performance data into readable, shareable takes. It is built for the space between a spreadsheet and a group chat: enough statistical rigor to be useful, but written in plain English and customizable voices.

The app compares a player's current-year stats against his weighted baseline, highlights what has actually changed, and asks the question fans and fantasy players usually care about: should this get better, worse, or stay about the same?

## About The Project

McFARLAND exists because advanced baseball stats are useful, but they are often trapped in tables, acronyms, and context that casual fans do not want to decode every morning. The app takes the numbers that matter, filters out noise where it can, and turns them into short baseball arguments.

What it does:

- Builds single-player hitter and pitcher breakdowns using current-year stats, weighted baselines, skill indicators, luck indicators, and sample-size context.
- Compares two or three hitters or pitchers side by side and recommends the stronger profile.
- Lets users choose an analysis "vibe" so the same data can sound straightforward, analytical, fantasy-focused, Gen Z, Shakespearean, sensationalist, optimistic, old-school, or cranky old-timer.
- Surfaces quick-link player groups like breakouts, on-fire players, ice-cold players, and out-of-character profiles.
- Provides weekly trend views based on stored daily trend snapshots.
- Includes an admin-only social assistant that suggests timely player links and draft posts for X and Bluesky.
- Produces share-friendly URLs and dynamic Open Graph/Twitter metadata for player and comparison links.

The name is a tribute to former Orioles lefty T.J. McFarland, patron saint of adapting to the situation and doing useful work without needing the spotlight.

## Current Tech Stack

McFARLAND is a TypeScript monorepo with three npm workspaces:

- `client`: React 18, Vite, React Router, TanStack React Query, React Markdown, CSS modules, Vitest, Testing Library.
- `server`: Node.js, Express, TypeScript, Zod validation, Helmet, CORS, rate limiting, OpenAI chat completions, PostgreSQL via `pg`, CSV parsing, Vitest, Supertest.
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
- `/api/analyze` and `/api/compare/analyze` build prompts from the current stats and selected vibe, then use OpenAI for headline + analysis generation.
- Successful analysis responses are cached in PostgreSQL by prompt hash, cache version, mode, and `dataThroughDate`, so repeat requests across users can skip OpenAI.
- Cache entries are valid only for the current `dataThroughDate`; stale rows are pruned opportunistically after successful writes.
- Analytics events are sent to Amplitude for sessions, analysis runs, and share actions. Admin-mode requests are excluded from analytics.
- Weekly trend storage uses PostgreSQL when `DATABASE_URL` is configured, with fallback behavior when storage is unavailable.

## Prerequisites

- Node.js 18+
- npm 9+
- Optional: OpenAI API key for generated analysis.
- Optional: PostgreSQL 17 or compatible Postgres database for shared analysis cache and weekly trend persistence.
- Optional: R runtime for running `refresh_data.R` locally.

## Getting Started

Install dependencies:

```bash
npm install
```

For local AI responses, set an OpenAI key:

```bash
export OPENAI_API_KEY="your-openai-key"
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
| `OPENAI_API_KEY` | Enables OpenAI-generated headline and analysis responses. | unset |
| `DATABASE_URL` | Enables Postgres-backed analysis cache and weekly trend storage. | unset |
| `AMPLITUDE_API_KEY` | Sends analytics events to a specific Amplitude project. | built-in project key |
| `ADMIN_PASSWORD` | Enables admin mode protection for social assistant endpoints. | unset |
| `TRENDS_REFRESH_SECRET` | Required secret for `/api/trends/refresh`. | unset |
| `TRENDS_TIMEZONE` | Timezone used for trend date calculations. | `America/Chicago` |
| `CLIENT_ORIGIN` | Comma-separated allowed CORS origins. | unset |
| `TRUST_PROXY` | Express trust proxy setting for deployed environments. | `1` |
| `PORT` | API/server port. | `3000` |

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
- Analysis cache validity follows `dataThroughDate`, not wall-clock date.
- Only successful structured OpenAI responses are cached. API errors, unavailable responses, and malformed model responses are not cached.
- The analysis cache is shared across users and sessions when Postgres is configured.
- Without `DATABASE_URL`, analysis still works, but shared cache and persisted weekly trend storage are disabled.

## License

Educational and personal use only. Respect applicable baseball data and image terms.
