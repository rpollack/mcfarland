import { useQuery } from "@tanstack/react-query";
import { fetchAbout } from "../api";
import styles from "../styles/AboutPage.module.css";

function AboutPage() {
  const { data, isLoading, error } = useQuery({ queryKey: ["about"], queryFn: fetchAbout });

  if (isLoading) {
    return <p className={styles.message}>Loading about information...</p>;
  }

  if (error || !data) {
    return <p className={styles.message}>Unable to load about content right now.</p>;
  }

  return (
    <article className={styles.article}>
      <h2>{data.heading}</h2>
      {data.paragraphs.map((paragraph, index) => (
        <p key={index}>{paragraph}</p>
      ))}
      <section className={styles.section}>
        <h3>What you can do</h3>
        <ul>
          <li>Run single-player analysis for hitters and pitchers with persona-driven AI commentary.</li>
          <li>Compare peers using core skill metrics and automatically surface the stronger profile.</li>
          <li>Switch vibes to tailor communication for front offices, fantasy players, or even Shakespeare.</li>
        </ul>
      </section>
      <section className={styles.section}>
        <h3>Under the hood</h3>
        <ul>
          <li>Node.js API securely loads curated CSV data and handles OpenAI requests with rate limiting.</li>
          <li>React UI powered by React Query keeps data fresh without extra clicks.</li>
          <li>GitHub Actions keeps the raw data refreshed via <code>refresh_data.R</code>.</li>
        </ul>
      </section>
    </article>
  );
}

export default AboutPage;
