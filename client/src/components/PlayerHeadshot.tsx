import { useState } from "react";
import clsx from "clsx";
import styles from "../styles/PlayerHeadshot.module.css";
import { buildFallbackHeadshotUrl, buildHeadshotUrl } from "../utils/headshots";

type Props = {
  name: string;
  playerId: string;
  mlbamid?: string | null;
  size?: number;
  className?: string;
};

export default function PlayerHeadshot({ name, playerId, mlbamid, size = 64, className }: Props) {
  const [src, setSrc] = useState(() => buildHeadshotUrl({ mlbamid, playerId }));
  const fallbackSrc = buildFallbackHeadshotUrl();

  return (
    <div className={clsx(styles.wrapper, className)} style={{ width: size, height: size }}>
      <img
        className={styles.image}
        src={src}
        alt={`${name} headshot`}
        loading="lazy"
        referrerPolicy="no-referrer"
        crossOrigin="anonymous"
        onError={() => {
          if (src !== fallbackSrc) {
            setSrc(fallbackSrc);
          }
        }}
      />
    </div>
  );
}
