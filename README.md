# McFARLAND ⚾

**Machine-crafted Forecasting And Reasoning for Luck, Analytics, Narratives, and Data**

McFARLAND now runs on a modern React front-end and a secure Node.js API. It keeps the original spirit—fast, explainable baseball analysis—while making it easy to extend, test, and deploy anywhere.

## Feature highlights
- **Single-player breakdowns** for hitters and pitchers with quick context and AI commentary.
- **Side-by-side comparisons** that recommend the stronger profile (xwOBA for hitters, xERA for pitchers).
- **Persona "vibes"** powered by OpenAI so every report speaks to your audience.
- **About page** describing the workflow and automatic data refresh pipeline.
- **Secure backend** with rate-limited endpoints and environment-based OpenAI access.

## Project structure
```
.
├── client/          # React + Vite UI (TypeScript)
├── server/          # Express API (TypeScript)
├── tests/e2e/       # Playwright end-to-end tests
├── full_stats_*.csv # Season data used by the API
├── player_lookup.csv
├── refresh_data.R   # GitHub Actions data refresh script (unchanged)
└── README.md
```

## Prerequisites
- Node.js 18+
- npm 9+
- (Optional) OpenAI API key for AI analysis

## Getting started
1. Install dependencies:
   ```bash
   npm install
   ```
2. (Optional) create an `.env` file at the project root or export the variable before starting:
   ```bash
   export OPENAI_API_KEY="your-openai-key"
   ```
3. Start the development environment (runs API on port 3000 and Vite on 5173):
   ```bash
   npm run dev
   ```
   Visit http://localhost:5173 to use the app.
4. To run the production build served by the Node API:
   ```bash
   npm run start
   ```
   The API will serve the pre-built React app on http://localhost:3000.

## Testing
- **Unit tests** (client + server):
  ```bash
  npm run test:unit
  ```
- **End-to-end tests** (requires the production build):
  ```bash
  npm run test:e2e
  ```

## Environment variables
| Name | Purpose | Default |
| ---- | ------- | ------- |
| `OPENAI_API_KEY` | Enables AI writeups. When omitted the UI shows a helpful notice instead of calling the API. | _(unset)_ |
| `CLIENT_ORIGIN` | Optional comma-separated list of allowed origins for CORS. | `*` |
| `PORT` | API port when running `npm run start`. | `3000` |

## Data pipeline
The Node API loads the CSV files in the repository root. `refresh_data.R` (still executed via GitHub Actions) regenerates those files on schedule, so you always have fresh data without touching the new codebase.

## Development tips
- The API caches OpenAI responses in-memory for one hour to control costs.
- React Query keeps the UI responsive and automatically refreshes data when inputs change.
- The E2E test spins up the production build, selects real players, and walks through the major flows to prevent regressions.

## License
Educational and personal use only. Respect FanGraphs' and MLB's data/image terms.
