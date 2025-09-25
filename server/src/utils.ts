export const CURRENT_YEAR = new Date().getFullYear();

export function safeNumber(value: unknown): number | null {
  if (value === null || value === undefined || value === "") {
    return null;
  }
  const num = Number(value);
  return Number.isFinite(num) ? num : null;
}

export function formatStatValue(value: number | null | undefined, decimals = 3): string {
  if (value === null || value === undefined || Number.isNaN(value)) {
    return "N/A";
  }
  if (Math.abs(value) < 1) {
    return Number(value).toFixed(decimals).replace(/^(-?)0+/, "$1");
  }
  return Number(value).toFixed(decimals);
}

export function formatPercentage(value: number | null | undefined, decimals = 1): string {
  if (value === null || value === undefined || Number.isNaN(value)) {
    return "N/A";
  }
  return `${Number(value).toFixed(decimals)}%`.replace(/^(-?)0+([0-9])/, "$1$2");
}

export function formatEra(value: number | null | undefined): string {
  if (value === null || value === undefined || Number.isNaN(value)) {
    return "N/A";
  }
  return Number(value).toFixed(2);
}

export function toTitleCase(text: string): string {
  return text
    .split("_")
    .map((part) => part.charAt(0).toUpperCase() + part.slice(1))
    .join(" ");
}

export function differenceLabel(diff: number | null | undefined): "up" | "down" | "flat" {
  if (diff === null || diff === undefined || Number.isNaN(diff) || Math.abs(diff) < 1e-6) {
    return "flat";
  }
  return diff > 0 ? "up" : "down";
}
