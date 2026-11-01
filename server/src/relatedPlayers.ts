import { listPlayers, getPlayerById } from "./dataStore.js";
import { getLuckStats, getSampleSize, MIN_FEATURED_SAMPLE } from "./luckGap.js";
import type { HitterRecord, PitcherRecord, PlayerType } from "./types.js";

const SIMILAR_GAP_COUNT = 2;

export interface RelatedPlayer {
  id: string;
  name: string;
  type: PlayerType;
  mlbamid: string | null;
  /** Luck gap, positive when results trail expectations. */
  gap: number | null;
}

export interface RelatedPlayers {
  /** Players whose results-vs-expectations gap is closest to this player's. */
  similarGap: RelatedPlayer[];
  /** A natural compare partner: the closest peer by expected performance. */
  rival: RelatedPlayer | null;
}

interface Profile {
  player: RelatedPlayer;
  /** Positive when results trail expectations (xwOBA - wOBA, or ERA - xERA). */
  gap: number | null;
  /** Expected performance (xwOBA or xERA), used to find a peer. */
  expected: number | null;
  eligible: boolean;
}

function buildProfile(type: PlayerType, record: HitterRecord | PitcherRecord): Profile {
  const { expected, gap } = getLuckStats(type, record);

  return {
    player: {
      id: String(record.PlayerId),
      name: record.Name,
      type,
      mlbamid: record.mlbamid ?? null,
      gap,
    },
    gap,
    expected,
    eligible: getSampleSize(type, record) >= MIN_FEATURED_SAMPLE,
  };
}

export function findRelatedPlayers(type: PlayerType, playerId: string): RelatedPlayers | null {
  const record = getPlayerById(type, playerId);
  if (!record) {
    return null;
  }

  const self = buildProfile(type, record);
  const peers = listPlayers(type)
    .map((peer) => buildProfile(type, peer))
    .filter((peer) => peer.player.id !== self.player.id && peer.eligible);

  const similarGap =
    self.gap === null
      ? []
      : peers
          .filter((peer) => peer.gap !== null)
          .sort((a, b) => Math.abs(a.gap! - self.gap!) - Math.abs(b.gap! - self.gap!))
          .slice(0, SIMILAR_GAP_COUNT);

  const takenIds = new Set(similarGap.map((peer) => peer.player.id));
  const rival =
    self.expected === null
      ? null
      : peers
          .filter((peer) => peer.expected !== null && !takenIds.has(peer.player.id))
          .sort((a, b) => Math.abs(a.expected! - self.expected!) - Math.abs(b.expected! - self.expected!))[0] ?? null;

  return {
    similarGap: similarGap.map((peer) => peer.player),
    rival: rival?.player ?? null,
  };
}
