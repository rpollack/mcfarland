export type VoteShare = { id: string; votes: number; percent: number };

/** Percentages that always add up to 100 (largest-remainder rounding), in the order of `ids`. */
export function computeVoteShares(ids: string[], counts: Record<string, number>): VoteShare[] {
  const total = ids.reduce((sum, id) => sum + (counts[id] ?? 0), 0);
  if (total === 0) {
    return ids.map((id) => ({ id, votes: 0, percent: 0 }));
  }

  const exact = ids.map((id) => ((counts[id] ?? 0) / total) * 100);
  const floors = exact.map(Math.floor);
  let remaining = 100 - floors.reduce((sum, value) => sum + value, 0);
  const byRemainder = exact
    .map((value, index) => ({ index, remainder: value - floors[index] }))
    .sort((a, b) => b.remainder - a.remainder || a.index - b.index);
  for (const { index } of byRemainder) {
    if (remaining <= 0) {
      break;
    }
    floors[index] += 1;
    remaining -= 1;
  }

  return ids.map((id, index) => ({ id, votes: counts[id] ?? 0, percent: floors[index] }));
}
