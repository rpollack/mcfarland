import { useEffect, useRef, useState } from "react";
import { Link } from "react-router-dom";
import { ANNOUNCEMENTS } from "../announcements";
import type { Announcement } from "../announcements";
import { logAnnouncementEvent } from "../api";
import { dismissAnnouncement, getActiveAnnouncement, readDismissedAnnouncementIds } from "../utils/announcements";
import styles from "../styles/AnnouncementBanner.module.css";

interface Props {
  announcements?: Announcement[];
  now?: Date;
}

function AnnouncementBanner({ announcements = ANNOUNCEMENTS, now }: Props) {
  const [dismissedIds, setDismissedIds] = useState(() => readDismissedAnnouncementIds());
  const [mountedAt] = useState(() => now ?? new Date());
  const announcement = getActiveAnnouncement(announcements, mountedAt, dismissedIds);
  const loggedViewRef = useRef<string | null>(null);

  const announcementId = announcement?.id ?? null;
  useEffect(() => {
    if (!announcementId || loggedViewRef.current === announcementId) {
      return;
    }
    loggedViewRef.current = announcementId;
    void logAnnouncementEvent(announcementId, "viewed");
  }, [announcementId]);

  if (!announcement) {
    return null;
  }

  const dismiss = (action: "clicked" | "dismissed") => {
    dismissAnnouncement(announcement.id);
    setDismissedIds((current) => new Set(current).add(announcement.id));
    void logAnnouncementEvent(announcement.id, action);
  };

  return (
    <div className={styles.banner} role="status">
      <p className={styles.message}>
        <strong>{announcement.lead}</strong> {announcement.text}
        {announcement.link && (
          <>
            {" "}
            <Link className={styles.link} to={announcement.link.to} onClick={() => dismiss("clicked")}>
              {announcement.link.label}
            </Link>
          </>
        )}
      </p>
      <button type="button" className={styles.dismiss} onClick={() => dismiss("dismissed")}>
        <span aria-hidden="true">×</span>
        <span className={styles.srOnly}>Dismiss announcement</span>
      </button>
    </div>
  );
}

export default AnnouncementBanner;
