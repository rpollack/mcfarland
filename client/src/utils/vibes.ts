/** Picks a random vibe other than the current one (or the only one, if there is just one). */
export function pickSurpriseVibe(vibeIds: string[], currentId: string, random: () => number = Math.random): string | null {
  const others = vibeIds.filter((id) => id !== currentId);
  const pool = others.length > 0 ? others : vibeIds;
  if (pool.length === 0) {
    return null;
  }
  return pool[Math.min(pool.length - 1, Math.floor(random() * pool.length))];
}
