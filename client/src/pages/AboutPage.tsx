import { useQuery } from "@tanstack/react-query";
import { Link } from "react-router-dom";
import { ANNOUNCEMENTS } from "../announcements";
import { fetchDataFreshness } from "../api";
import { getAnnouncementHistory } from "../utils/announcements";
import styles from "../styles/AboutPage.module.css";

const ABOUT_SECTIONS = [
  {
    title: "What you'll find",
    items: [
      "<strong>Single-player takes</strong> with a headline, the key stat signals, and a plain-English read in the vibe you choose.",
      "<strong>The mirage board</strong> on the home screen lists the hitters or pitchers whose season results ran furthest ahead of, or behind, what their skills earned. A short intro is written in your vibe, and tapping a player opens their take.",
      "<strong>Keep going</strong> suggestions at the end of a take: players with a similar luck gap, and a natural rival to compare against.",
      "<strong>Compare mode</strong> lines up two or three hitters or pitchers so you know who gets the nod and who hits the pine. Then pick who you'd take and see how everyone else voted.",
      "<strong>Fantasy Tools</strong> give you a start/sit call for a player using today's MLB matchup.",
      "<strong>Vibes</strong> (personality toggles) let the same data sound like a front office nerd, a hype broadcast, or Shakespeare with pine tar. Tap any voice under a take, or hit Surprise me.",
      "<strong>Stars</strong> let you mark players you care about. Your starred players are saved in this browser only.",
      "<strong>Share cards</strong>: a shared link previews with the headline, the player or players, and the voice it was written in.",
    ],
  },
  {
    title: "Reading the luck numbers",
    items: [
      "Hitters: <strong>wOBA</strong> is what a hitter actually produced, and <strong>xwOBA</strong> is what his quality of contact says he should have produced.",
      "Pitchers: <strong>ERA</strong> is what a pitcher actually allowed, and <strong>xERA</strong> is what the quality of contact and his strikeouts and walks say he should have allowed.",
      "The <strong>luck gap</strong> is the difference between the two. A positive gap means the results trail the skills, and a negative gap means the results ran ahead of them.",
      "Gaps often shrink over time, which is why they are useful, but one season of results is still a small sample. Treat a big gap as a reason to look closer, not a prediction.",
    ],
  },
  {
    title: "How quick links work",
    items: [
      "\"In the News\" highlights players showing up in the current MLB news cycle, then uses McFARLAND stats to find the best analysis angle.",
      "\"Breakouts\" highlights players whose underlying skill stats look meaningfully better than their weighted baseline and the league-wide trend, while discounting tiny samples and obvious luck spikes.",
      "\"On Fire\" shows the players whose recent trend improved the most over the last 7 days compared to the 7 days before that.",
      "\"Ice Cold\" shows the players whose recent trend moved hardest in the wrong direction over that same window.",
      "\"Out of Character\" is a daily watch list of players whose current-year stats look the most different from their weighted baseline after accounting for league-wide changes.",
    ],
  },
  {
    title: "Why it exists",
    items: [
      "Advanced stats shouldn't require a graduate seminar. McFARLAND translates them into friendly stories you can drop in chats or fantasy forums.",
      "Fresh info every morning in-season, right after last night's games, so you're never flexing stale numbers.",
      "Named for former Orioles swiss-army reliever T.J. McFarland, our mascot for adaptability.",
    ],
  },
  {
    title: "Your data",
    items: [
      "You never need an account. Starred players are saved in your browser and nowhere else.",
      "Votes are one per browser. Your browser keeps a random id, and the server stores only a scrambled version of it with your vote. It is not your name, and it is separate from the id used for visit counts.",
      "We use Amplitude to count visits and which features get used, such as takes run, links shared, player searches (what you typed and how many matches it found), mirage board views and clicks, and announcement views and clicks, so we know what to improve.",
    ],
  },
];

const VIBE_BLURBS = [
  "➡️ Straightforward keeps it warm, clear, and grounded.",
  "📊 Analytics Nerd leans into probabilities, evidence, and the sharpest supporting metrics.",
  "🍗 Fantasy Expert turns the same analysis into lineup and roster advice.",
  "🌀 Gen Z fires off memes, slang, and hype like a late-night group chat.",
  "🎷 Old-School writes like a vintage baseball columnist with plenty of era flavor.",
  "🧓 Old-Timer grumbles about the modern game while still making a coherent point.",
  "🌹 Optimist starts with the encouraging signs but still has to acknowledge real risk.",
  "🎪 Sensationalist makes every at-bat sound like the pennant is on the line.",
  "🎭 Shakespeare brings dramatic flair without losing the thread.",
];

function formatAnnouncementDate(startsAt: string): string {
  return new Date(`${startsAt}T00:00:00Z`).toLocaleDateString("en-US", {
    month: "short",
    day: "numeric",
    year: "numeric",
    timeZone: "UTC",
  });
}

function AboutPage() {
  const freshnessQuery = useQuery({
    queryKey: ["data-freshness"],
    queryFn: fetchDataFreshness,
  });
  const announcementHistory = getAnnouncementHistory(ANNOUNCEMENTS, new Date());

  return (
    <article className={styles.article}>
      <h2>🤖⚾ McFARLAND</h2>
      <p className={styles.intro}>
        <strong>Machine-crafted Forecasting And Reasoning for Luck, Analytics, Narratives, and Data.</strong> Call it McFARLAND for short. It's your robot buddy who turns stacks of baseball numbers into something you can actually talk about with friends.
      </p>
      <p className={styles.intro}>
        The app updates every morning during the season, right after the previous night's games. You get insights that are fresh off the field and ready for your fantasy lineup or group chat brag.
      </p>

      <section className={styles.section}>
        <h3>How the analysis works</h3>
        <p className={styles.bodyCopy}>
          For each player, we take his current-season stats and compare them to a weighted baseline, so the app can highlight what has actually changed instead of just repeating raw numbers.
        </p>
        <p className={styles.bodyCopy}>
          The weighted baseline uses the previous three seasons with 5/3/1 recency weights: last year counts most, two years ago counts less, and three years ago counts least. It is also weighted by playing time, using PA for hitters and batters faced for pitchers.
        </p>
        <p className={styles.bodyCopy}>
          McFARLAND also computes the league-wide version of that weighted baseline for the same metrics, then compares each player's change against the league's change. That helps separate player-specific growth or decline from broader shifts in the run environment, baseball, rules, scoring, or league-wide approach.
        </p>
        <p className={styles.bodyCopy}>
          From there, McFARLAND separates skill signals from luck signals, accounts for age, sample size, and league-adjusted trends, and turns the results into plain-English analysis.
        </p>
        {freshnessQuery.data ? (
          <p className={styles.freshnessLine}>
            Data is current through games on {freshnessQuery.data.dataThroughLabel}.
          </p>
        ) : null}
      </section>

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
          The data stays the same. The storyteller changes. Tap any voice under a take to hear it again, or let Surprise me pick one. The voice shifts from a hype squad to a stat lab to a bard with a glove.
        </p>
        <ul>
          {VIBE_BLURBS.map((blurb) => (
            <li key={blurb}>{blurb}</li>
          ))}
        </ul>
      </section>

      {announcementHistory.length > 0 && (
        <section className={styles.section} aria-label="What's new">
          <h3>What&apos;s new</h3>
          <ul>
            {announcementHistory.map((announcement) => (
              <li key={announcement.id}>
                <strong>{formatAnnouncementDate(announcement.startsAt)}.</strong> {announcement.text}
                {announcement.link && (
                  <>
                    {" "}
                    <Link to={announcement.link.to}>{announcement.link.label}</Link>
                  </>
                )}
              </li>
            ))}
          </ul>
        </section>
      )}

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

      <p className={styles.attribution}>
        MLB data © MLB Advanced Media, L.P. Use of MLB data is subject to{" "}
        <a href="http://gdx.mlb.com/components/copyright.txt" target="_blank" rel="noreferrer">
          MLB&apos;s terms
        </a>
        .
      </p>
    </article>
  );
}

export default AboutPage;
