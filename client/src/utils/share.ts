export function buildSharePreviewUrl(currentUrl: string): string {
  const url = new URL(currentUrl);
  url.pathname = "/share";
  url.hash = "";
  return url.toString();
}

