import { useState } from "react";
import type { CSSProperties } from "react";
import clsx from "clsx";
import styles from "../styles/PlayerHeadshot.module.css";
import { buildFallbackHeadshotUrl, buildHeadshotUrl } from "../utils/headshots";

type Props = {
  name: string;
  mlbamid?: string | null;
  size?: number;
  className?: string;
};

export default function PlayerHeadshot({ name, mlbamid, size = 64, className }: Props) {
  const [failedSrc, setFailedSrc] = useState<string | null>(null);
  const fallbackSrc = buildFallbackHeadshotUrl();
  const desiredSrc = buildHeadshotUrl(mlbamid);
  const src = failedSrc === desiredSrc ? fallbackSrc : desiredSrc;

  return (
    <div
      className={clsx(styles.wrapper, className)}
      style={{ "--headshot-size": `${size}px` } as CSSProperties}
    >
      <img
        className={styles.image}
        src={src}
        alt={`${name} headshot`}
        loading="lazy"
        referrerPolicy="no-referrer"
        crossOrigin="anonymous"
        onError={() => {
          if (src !== fallbackSrc) {
            setFailedSrc(desiredSrc);
          }
        }}
      />
    </div>
  );
}
