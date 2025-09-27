const MLB_PHOTO_BASE =
  "https://img.mlbstatic.com/mlb-photos/image/upload/w_213,d_people:generic:headshot:silo:current.png,q_auto:best,f_auto/v1/people";
const MLB_GENERIC_HEADSHOT = `${MLB_PHOTO_BASE}/0/headshot/67/current`;

export function buildHeadshotUrl({
  mlbamid,
  playerId,
}: {
  mlbamid?: string | null;
  playerId: string;
}): string {
  if (mlbamid && mlbamid.trim().length > 0 && mlbamid !== "0") {
    return `${MLB_PHOTO_BASE}/${mlbamid}/headshot/67/current`;
  }
  return MLB_GENERIC_HEADSHOT;
}

export function buildFallbackHeadshotUrl(): string {
  return MLB_GENERIC_HEADSHOT;
}
