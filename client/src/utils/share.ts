export function buildSharePreviewUrl(currentUrl: string): string {
  const url = new URL(currentUrl);
  url.pathname = "/share";
  url.hash = "";

  const mode = url.searchParams.get("mode");
  const playerId = url.searchParams.get("playerId");
  const playerIds = url.searchParams.get("playerIds");
  const playerType = url.searchParams.get("playerType");
  const vibe = url.searchParams.get("vibe");

  const hasPlayerSelection = Boolean(playerId) || Boolean(playerIds);
  const isDefaultSingleState =
    mode === "single" &&
    !hasPlayerSelection &&
    (playerType === null || playerType === "hitter") &&
    (vibe === null || vibe === "" || vibe === "straightforward");

  if (isDefaultSingleState) {
    url.search = "";
    return url.toString();
  }

  return url.toString();
}
