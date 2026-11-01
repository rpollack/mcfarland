import fs from "node:fs";
import path from "node:path";
import { createRequire } from "node:module";
import { Resvg } from "@resvg/resvg-js";
import satori from "satori";
import type { HitterRecord, PitcherRecord, PlayerType } from "./types.js";

const WIDTH = 1200;
const HEIGHT = 630;
const MAX_CACHED_IMAGES = 200;
const HEADSHOT_TIMEOUT_MS = 2500;
const MLB_HEADSHOT_PNG =
  "https://img.mlbstatic.com/mlb-photos/image/upload/w_240,d_people:generic:headshot:silo:current.png,q_auto:best,f_png/v1/people";

export interface ShareImageSubject {
  name: string;
  mlbamid: string | null;
}

export interface ShareImageModel {
  mode: "single" | "compare";
  headline: string;
  vibeLabel: string;
  subjects: ShareImageSubject[];
  /** Single-player only: the stat that explains the take, e.g. "xwOBA .396 vs. wOBA .372". */
  statLine: string | null;
  prompt: string;
  dataThroughLabel: string;
}

type Player = HitterRecord | PitcherRecord;

function finite(value: number | null | undefined): number | null {
  return value !== null && value !== undefined && Number.isFinite(value) ? value : null;
}

function formatRate(value: number): string {
  return value.toFixed(3).replace(/^0/, "");
}

function buildStatLine(player: Player, playerType: PlayerType): string | null {
  if (playerType === "hitter") {
    const hitter = player as HitterRecord;
    const expected = finite(hitter.xwOBA_cur);
    const actual = finite(hitter.wOBA_cur);
    return expected !== null && actual !== null ? `xwOBA ${formatRate(expected)} vs. wOBA ${formatRate(actual)}` : null;
  }
  const pitcher = player as PitcherRecord;
  const expected = finite(pitcher.xera_cur);
  const actual = finite(pitcher.era_cur);
  return expected !== null && actual !== null ? `xERA ${expected.toFixed(2)} vs. ERA ${actual.toFixed(2)}` : null;
}

function joinWithOr(names: string[]): string {
  if (names.length <= 2) {
    return names.join(" or ");
  }
  return `${names.slice(0, -1).join(", ")}, or ${names[names.length - 1]}`;
}

function truncate(value: string, maxLength: number): string {
  const clean = value.replace(/\s+/g, " ").trim();
  return clean.length <= maxLength ? clean : `${clean.slice(0, maxLength - 1).trimEnd()}…`;
}

export function buildShareImageModel(input: {
  mode: "single" | "compare";
  playerType: PlayerType;
  vibeLabel: string;
  players: Player[];
  headline: string | null;
  dataThroughLabel: string;
}): ShareImageModel {
  const { mode, playerType, players } = input;
  const subjects = players.map((player) => ({ name: player.Name, mlbamid: player.mlbamid ?? null }));
  const fallbackHeadline =
    mode === "compare"
      ? `Who would you take: ${joinWithOr(subjects.map((subject) => subject.name))}?`
      : `${subjects[0]?.name ?? ""} ${playerType === "pitcher" ? "Pitching" : "Hitting"} Breakdown`;

  return {
    mode,
    headline: truncate(input.headline ?? fallbackHeadline, 140),
    vibeLabel: input.vibeLabel,
    subjects,
    statLine: mode === "single" && players[0] ? buildStatLine(players[0], playerType) : null,
    prompt: mode === "compare" ? "Cast your vote and see how everyone else picked" : "What the numbers say, in plain English",
    dataThroughLabel: input.dataThroughLabel,
  };
}

let fontsPromise: Promise<{ name: string; data: Buffer; weight: 500 | 700; style: "normal" }[]> | null = null;

function loadFonts() {
  if (!fontsPromise) {
    fontsPromise = (async () => {
      const require = createRequire(import.meta.url);
      const filesDir = path.join(path.dirname(require.resolve("@fontsource/inter/package.json")), "files");
      const files: { file: string; weight: 500 | 700 }[] = [
        { file: "inter-latin-700-normal.woff", weight: 700 },
        { file: "inter-latin-ext-700-normal.woff", weight: 700 },
        { file: "inter-latin-500-normal.woff", weight: 500 },
        { file: "inter-latin-ext-500-normal.woff", weight: 500 },
      ];
      return files.map(({ file, weight }) => ({
        name: "Inter",
        data: fs.readFileSync(path.join(filesDir, file)),
        weight,
        style: "normal" as const,
      }));
    })();
    fontsPromise.catch(() => {
      fontsPromise = null;
    });
  }
  return fontsPromise;
}

async function fetchHeadshotDataUri(mlbamid: string | null): Promise<string | null> {
  if (!mlbamid || mlbamid.trim() === "" || mlbamid === "0" || !/^\d+$/.test(mlbamid)) {
    return null;
  }
  try {
    const response = await fetch(`${MLB_HEADSHOT_PNG}/${mlbamid}/headshot/67/current`, {
      signal: AbortSignal.timeout(HEADSHOT_TIMEOUT_MS),
    });
    if (!response.ok || !(response.headers.get("content-type") ?? "").startsWith("image/png")) {
      return null;
    }
    const bytes = Buffer.from(await response.arrayBuffer());
    return `data:image/png;base64,${bytes.toString("base64")}`;
  } catch {
    return null;
  }
}

type Node = { type: string; props: Record<string, unknown> };

// Satori needs every element with more than one child to be a flex container.
function el(type: string, style: Record<string, unknown>, children?: unknown): Node {
  return { type, props: { style: { display: "flex", ...style }, children } };
}

function initials(name: string): string {
  return name
    .split(/\s+/)
    .filter(Boolean)
    .slice(0, 2)
    .map((part) => part[0]?.toUpperCase() ?? "")
    .join("");
}

function avatar(subject: ShareImageSubject, headshot: string | null, size: number): Node {
  const frame = {
    width: size,
    height: size,
    borderRadius: size / 2,
    border: "4px solid rgba(255,255,255,0.85)",
    overflow: "hidden",
    flex: "none",
  };
  if (headshot) {
    return el("div", { ...frame, background: "#2f54eb" }, {
      type: "img",
      props: { src: headshot, width: size, height: size, style: { width: size, height: size, objectFit: "cover" } },
    });
  }
  return el(
    "div",
    { ...frame, background: "#2f54eb", alignItems: "center", justifyContent: "center", fontSize: size * 0.38, fontWeight: 700 },
    initials(subject.name)
  );
}

function headlineSize(headline: string): number {
  if (headline.length <= 55) return 68;
  if (headline.length <= 85) return 58;
  if (headline.length <= 115) return 50;
  return 44;
}

function buildTree(model: ShareImageModel, headshots: (string | null)[]): Node {
  const compare = model.mode === "compare";
  const crowded = compare && model.subjects.length > 2;
  const avatarSize = !compare ? 120 : crowded ? 76 : 96;

  const subjectRows: Node[] = [];
  model.subjects.forEach((subject, index) => {
    if (compare && index > 0) {
      subjectRows.push(el("div", { fontSize: 26, fontWeight: 500, color: "#9fb0e6", margin: crowded ? "0 16px" : "0 28px" }, "vs."));
    }
    subjectRows.push(
      el("div", { alignItems: "center", gap: crowded ? 14 : 18 }, [
        avatar(subject, headshots[index] ?? null, avatarSize),
        el(
          "div",
          { flexDirection: "column", justifyContent: "center", maxWidth: !compare ? 640 : crowded ? 190 : 300 },
          [
            el("div", { fontSize: !compare ? 46 : crowded ? 26 : 34, fontWeight: 700, lineHeight: 1.1 }, subject.name),
            ...(!compare && model.statLine
              ? [el("div", { fontSize: 28, fontWeight: 500, color: "#9fb0e6", marginTop: 8 }, model.statLine)]
              : []),
          ]
        ),
      ])
    );
  });

  return el(
    "div",
    {
      width: WIDTH,
      height: HEIGHT,
      boxSizing: "border-box",
      flexDirection: "column",
      justifyContent: "space-between",
      padding: "52px 64px",
      color: "#ffffff",
      fontFamily: "Inter",
      background: "linear-gradient(135deg, #15213b 0%, #1e3a8a 100%)",
    },
    [
      el("div", { justifyContent: "space-between", alignItems: "center" }, [
        el("div", { fontSize: 28, fontWeight: 700, letterSpacing: 5, color: "#9fb0e6" }, "McFARLAND"),
        el(
          "div",
          { padding: "8px 22px", borderRadius: 999, background: "rgba(255,255,255,0.14)", fontSize: 26, fontWeight: 700 },
          model.vibeLabel
        ),
      ]),
      el(
        "div",
        { flex: 1, alignItems: "center", fontSize: headlineSize(model.headline), fontWeight: 700, lineHeight: 1.12, maxWidth: 1060 },
        model.headline
      ),
      el("div", { flexDirection: "column", gap: 20 }, [
        el("div", { alignItems: "center" }, subjectRows),
        el("div", { fontSize: 24, fontWeight: 500, color: "#9fb0e6" }, `${model.prompt} · Stats through ${model.dataThroughLabel}`),
      ]),
    ]
  );
}

const imageCache = new Map<string, Buffer>();

export async function renderShareImage(model: ShareImageModel): Promise<Buffer> {
  const cacheKey = JSON.stringify(model);
  const cached = imageCache.get(cacheKey);
  if (cached) {
    return cached;
  }

  const [fonts, headshots] = await Promise.all([
    loadFonts(),
    Promise.all(model.subjects.map((subject) => fetchHeadshotDataUri(subject.mlbamid))),
  ]);
  const svg = await satori(buildTree(model, headshots) as unknown as Parameters<typeof satori>[0], {
    width: WIDTH,
    height: HEIGHT,
    fonts,
  });
  const png = new Resvg(svg, { fitTo: { mode: "width", value: WIDTH } }).render().asPng();

  // Only remember images whose headshots all loaded, so a transient MLB outage is not cached.
  if (headshots.every((headshot, index) => headshot || !model.subjects[index].mlbamid)) {
    if (imageCache.size >= MAX_CACHED_IMAGES) {
      const oldest = imageCache.keys().next().value;
      if (oldest !== undefined) {
        imageCache.delete(oldest);
      }
    }
    imageCache.set(cacheKey, png);
  }
  return png;
}

export function __clearShareImageCacheForTests(): void {
  imageCache.clear();
}
