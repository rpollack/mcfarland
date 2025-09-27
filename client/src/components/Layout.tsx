import { NavLink } from "react-router-dom";
import VibeSelector from "./VibeSelector";
import styles from "../styles/Layout.module.css";

function Layout({ children }: { children: React.ReactNode }) {
  return (
    <div className={styles.appContainer}>
      <header className={styles.header}>
        <div className={styles.branding}>
          <h1>McFarland</h1>
          <span className={styles.tagline}>Modern baseball intelligence</span>
        </div>
        <VibeSelector />
      </header>
      <nav className={styles.nav}>
        <NavLink to="/" className={({ isActive }) => (isActive ? styles.activeLink : styles.link)} end>
          Single Player
        </NavLink>
        <NavLink to="/compare" className={({ isActive }) => (isActive ? styles.activeLink : styles.link)}>
          Compare Players
        </NavLink>
        <NavLink to="/about" className={({ isActive }) => (isActive ? styles.activeLink : styles.link)}>
          About
        </NavLink>
      </nav>
      <main className={styles.main}>{children}</main>
      <footer className={styles.footer}>
        Data auto-refreshes via GitHub Actions. Built with React + Node.js.
      </footer>
    </div>
  );
}

export default Layout;
