import { useEffect, useRef } from "react";
import { logUiEvent } from "../api";
import type { SearchSurface } from "../api";
import type { PlayerType } from "../types";

// Search queries are typed text, so cap what we send to Amplitude.
const MAX_QUERY_LENGTH = 64;

export function toLoggedQuery(term: string): string {
  return term.trim().slice(0, MAX_QUERY_LENGTH);
}

/**
 * Logs one `player_search_performed` event per settled search, including searches with no results.
 * `resultCount` is undefined until results for `term` have loaded and the debounce has caught up.
 */
export function useSearchLogging({
  surface,
  playerType,
  term,
  resultCount,
}: {
  surface: SearchSurface;
  playerType: PlayerType;
  term: string;
  resultCount: number | undefined;
}) {
  const lastLoggedKeyRef = useRef<string | null>(null);

  useEffect(() => {
    if (resultCount === undefined) {
      lastLoggedKeyRef.current = null;
      return;
    }

    const key = `${surface}|${playerType}|${term}`;
    if (lastLoggedKeyRef.current === key) {
      return;
    }
    lastLoggedKeyRef.current = key;
    void logUiEvent({
      event: "player_search_performed",
      surface,
      playerType,
      query: toLoggedQuery(term),
      resultCount,
    });
  }, [surface, playerType, term, resultCount]);
}
