export const metadata = {
  title: 'About McFARLAND'
};

export default function AboutPage() {
  return (
    <div className="space-y-8">
      <section className="prose prose-slate max-w-3xl">
        <h1>About the rebuild</h1>
        <p>
          McFARLAND now ships as a decoupled stack: a Plumber API that surfaces cached FanGraphs data and
          persona-ready prompts, plus a responsive React front end that keeps the experience stable on mobile.
        </p>
        <p>
          The API mirrors the legacy Shiny helpers and exposes endpoints for player lookup, statlines, persona
          analysis, and multi-player comparisons. The Next.js client calls those routes with SWR, renders stat
          tables with TailwindCSS, and preserves the old deep-link behaviour via URL query parameters.
        </p>
      </section>

      <section className="prose prose-slate max-w-3xl">
        <h2>Key endpoints</h2>
        <ul>
          <li>
            <code>GET /players</code> — pageless roster search with optional <code>type</code> and <code>search</code>{' '}
            filters.
          </li>
          <li>
            <code>GET /players/&lt;id&gt;/statline</code> — formatted 2025 vs. three-year stat grid for any player.
          </li>
          <li>
            <code>GET /players/&lt;id&gt;/analysis</code> — persona-aware OpenAI commentary.
          </li>
          <li>
            <code>POST /compare</code> — ranked comparison with a recommended player and AI summary.
          </li>
        </ul>
        <p>
          Deploy the API with <code>plumber::pr('services/api/plumber.R')</code> and serve the web app with{' '}
          <code>npm run dev</code>. Both layers respect the <code>OPENAI_API_KEY</code> and{' '}
          <code>MCFARLAND_DATA_URL</code> environment variables.
        </p>
      </section>
    </div>
  );
}
