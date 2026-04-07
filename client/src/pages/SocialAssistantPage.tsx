import { useMemo, useState } from "react";
import { useQuery } from "@tanstack/react-query";
import { fetchSocialSuggestions } from "../api";
import styles from "../styles/SocialAssistantPage.module.css";

function CopyButton({ text }: { text: string }) {
  const [copied, setCopied] = useState(false);

  return (
    <button
      type="button"
      className={styles.copyButton}
      onClick={async () => {
        try {
          await navigator.clipboard.writeText(text);
          setCopied(true);
          window.setTimeout(() => setCopied(false), 1800);
        } catch (_error) {
          setCopied(false);
        }
      }}
    >
      {copied ? "Copied" : "Copy"}
    </button>
  );
}

function SocialAssistantPage() {
  const query = useQuery({
    queryKey: ["social-suggestions"],
    queryFn: fetchSocialSuggestions,
  });

  const recommendedId = query.data?.recommended.playerId;
  const generatedLabel = useMemo(() => {
    if (!query.data?.generatedAt) {
      return undefined;
    }

    const date = new Date(query.data.generatedAt);
    return new Intl.DateTimeFormat("en-US", {
      month: "short",
      day: "numeric",
      hour: "numeric",
      minute: "2-digit",
    }).format(date);
  }, [query.data?.generatedAt]);

  if (query.isLoading) {
    return (
      <section className={styles.panel}>
        <h2>Social Assistant</h2>
        <p className={styles.message}>Pulling together timely player ideas…</p>
      </section>
    );
  }

  if (query.isError || !query.data) {
    return (
      <section className={styles.panel}>
        <h2>Social Assistant</h2>
        <p className={styles.message}>
          Couldn&apos;t generate suggestions. Make sure you&apos;re in admin mode and try again.
        </p>
      </section>
    );
  }

  return (
    <section className={styles.panel}>
      <div className={styles.headerRow}>
        <div>
          <h2>Social Assistant</h2>
          <p className={styles.subhead}>
            Three player ideas for today, one recommendation, and ready-to-tweak posts.
          </p>
        </div>
        <button type="button" className={styles.refreshButton} onClick={() => void query.refetch()}>
          Refresh
        </button>
      </div>

      <div className={styles.metaRow}>
        <span>Through games on {query.data.dataThroughLabel}</span>
        {generatedLabel ? <span>Generated {generatedLabel}</span> : null}
        <span>{query.data.usedFallback ? "Fallback mode" : "AI-ranked"}</span>
      </div>

      <div className={styles.summaryCard}>
        <h3>Best post today: {query.data.recommended.playerName}</h3>
        <p>{query.data.recommended.recommendationWhy}</p>
        <a href={query.data.recommended.shareUrl} target="_blank" rel="noreferrer">
          Open share link →
        </a>
      </div>

      <div className={styles.candidateGrid}>
        {query.data.candidates.map((candidate) => (
          <article
            key={`${candidate.playerType}-${candidate.playerId}`}
            className={candidate.playerId === recommendedId ? styles.recommendedCard : styles.candidateCard}
          >
            <div className={styles.candidateHeader}>
              <div>
                <h3>{candidate.playerName}</h3>
                <p className={styles.playerType}>{candidate.playerType}</p>
              </div>
              {candidate.playerId === recommendedId ? <span className={styles.badge}>Recommended</span> : null}
            </div>
            <p className={styles.whyNow}>{candidate.whyNow}</p>
            <p className={styles.snapshot}>{candidate.statSnapshot}</p>
            {candidate.news.length > 0 ? (
              <ul className={styles.newsList}>
                {candidate.news.map((item) => (
                  <li key={`${candidate.playerId}-${item.link}`}>
                    <a href={item.link} target="_blank" rel="noreferrer">
                      {item.title}
                    </a>
                    {item.source ? <span className={styles.newsSource}>{item.source}</span> : null}
                  </li>
                ))}
              </ul>
            ) : (
              <p className={styles.noNews}>No strong headline found, but the stats still make this player interesting.</p>
            )}
          </article>
        ))}
      </div>

      <div className={styles.draftsGrid}>
        <section className={styles.draftCard}>
          <div className={styles.draftHeader}>
            <h3>X drafts</h3>
          </div>
          {query.data.xPosts.map((post, index) => (
            <div key={`x-${index}`} className={styles.draftBlock}>
              <p>{post}</p>
              <CopyButton text={post} />
            </div>
          ))}
        </section>

        <section className={styles.draftCard}>
          <div className={styles.draftHeader}>
            <h3>Bluesky drafts</h3>
          </div>
          {query.data.blueskyPosts.map((post, index) => (
            <div key={`bsky-${index}`} className={styles.draftBlock}>
              <p>{post}</p>
              <CopyButton text={post} />
            </div>
          ))}
        </section>
      </div>

      <section className={styles.summaryFooter}>
        <h3>Why these suggestions</h3>
        <p>{query.data.modelSummary}</p>
      </section>
    </section>
  );
}

export default SocialAssistantPage;
