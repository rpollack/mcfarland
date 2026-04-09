import { useQuery } from "@tanstack/react-query";
import { fetchDataFreshness } from "../api";
import styles from "../styles/AboutPage.module.css";

const ABOUT_SECTIONS = [
  {
    title: "What you'll find",
    items: [
      "Single-player scouting notes that mix quick-hit insights with your chosen vibe.",
      "Compare mode that lines up hitters or pitchers so you know who gets the nod and who hits the pine.",
      "Personality toggles (aka vibes) that let the same data sound like a front office nerd, a hype broadcast, or Shakespeare with pine tar.",
    ],
  },
  {
    title: "How quick links work",
    items: [
      "\"Breakouts\" highlights players whose skill-oriented stats look meaningfully better than their recent baseline, with enough sample to take seriously.",
      "\"On Fire\" shows the players whose recent trend improved the most over the last 7 days compared to the 7 days before that.",
      "\"Ice Cold\" shows the players whose recent trend moved hardest in the wrong direction over that same window.",
      "\"Out of Character\" is a daily watch list of players whose current-year stats look the most different from their recent baseline.",
    ],
  },
  {
    title: "Why it exists",
    items: [
      "Advanced stats shouldn't require a graduate seminar. McFARLAND translates them into friendly stories you can drop in chats or fantasy forums.",
      "Fresh info every morning in-season, right after last night's games, so you're never flexing stale numbers.",
      "Named for former Orioles swiss-army reliever T.J. McFarland, our mascot for adaptability. <a href=\"https://www.fangraphs.com/players/tj-mcfarland/3237/stats?position=P\" target=\"_blank\" rel=\"noreferrer\">Give the man his due</a>.",
    ],
  },
];

const VIBE_BLURBS = [
  "➡️ Straightforward keeps it clean and scouts-ready.",
  "📊 Analytics Nerd leans into probabilities, evidence, and the sharpest supporting metrics.",
  "🍗 Fantasy Expert turns the same analysis into lineup and roster advice.",
  "🌀 Gen Z fires off memes, slang, and hype like a late-night group chat.",
  "🎷 Old-School writes like a vintage baseball columnist with plenty of era flavor.",
  "🧓 Old-Timer grumbles about the modern game while still making a coherent point.",
  "🌹 Optimist starts with the encouraging signs but still has to acknowledge real risk.",
  "🎪 Sensationalist makes every at-bat sound like the pennant is on the line.",
  "🎭 Shakespeare brings dramatic flair without losing the thread.",
];

function AboutPage() {
  const freshnessQuery = useQuery({
    queryKey: ["data-freshness"],
    queryFn: fetchDataFreshness,
  });

  return (
    <article className={styles.article}>
      <h2>🤖⚾ McFARLAND</h2>
      <p className={styles.intro}>
        <strong>Machine-crafted Forecasting And Reasoning for Luck, Analytics, Narratives, and Data.</strong> Call it McFARLAND for short. It's your robot buddy who turns stacks of baseball numbers into something you can actually talk about with friends.
      </p>
      <p className={styles.intro}>
        The app updates every morning during the season, right after the previous night's games. You get insights that are fresh off the field and ready for your fantasy lineup or group chat brag.
      </p>

      {ABOUT_SECTIONS.map((section) => (
        <section key={section.title} className={styles.section}>
          <h3>{section.title}</h3>
          <ul>
            {section.items.map((item) => (
              <li key={item} dangerouslySetInnerHTML={{ __html: item }} />
            ))}
          </ul>
        </section>
      ))}

      <section className={styles.vibes}>
        <h3>Pick a vibe, change the story</h3>
        <p>
          The data stays the same. The storyteller changes. Try every vibe to see how the voice shifts—from a hype squad to a stat lab to a bard with a glove.
        </p>
        <ul>
          {VIBE_BLURBS.map((blurb) => (
            <li key={blurb}>{blurb}</li>
          ))}
        </ul>
      </section>

      <section className={styles.section}>
        <h3>How the data stays current</h3>
        <p className={styles.bodyCopy}>
          McFARLAND refreshes automatically every morning during the season using fresh baseball data. Each player's current-year performance is compared against his recent baseline, so the app can highlight what has actually changed instead of just repeating raw stats.
        </p>
        {freshnessQuery.data ? (
          <p className={styles.freshnessLine}>
            Data is current through games on {freshnessQuery.data.dataThroughLabel}.
          </p>
        ) : null}
      </section>

      <section className={styles.section}>
        <h3>Help shape McFARLAND</h3>
        <p>
          Got a vibe idea, stat wish, or bug report? Drop it in the feedback form and we’ll take a look.
          <br />
          <a
            href="https://docs.google.com/forms/d/12MJvsOGi8p8TSIaFLe2xbPC8pSmAzNk3QceX6j12XqU/edit"
            target="_blank"
            rel="noreferrer"
          >
            Share your feedback →
          </a>
        </p>
      </section>
    </article>
  );
}

export default AboutPage;
