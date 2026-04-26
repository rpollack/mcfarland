import type {
  DailyMatchupContext,
  FantasyPitcherContext,
  FantasyTeamContext,
  HitterRecord,
  PitcherRecord,
  PlayerType,
} from "./types.js";

const MLB_API_BASE = "https://statsapi.mlb.com/api";
const CACHE_TTL_MS = 5 * 60 * 1000;

type CacheEntry<T> = {
  expiresAt: number;
  value: T;
};

type MlbPerson = {
  id: number;
  fullName: string;
  currentTeam?: { id: number; name: string };
  batSide?: { code?: string; description?: string };
  pitchHand?: { code?: string; description?: string };
};

type MlbScheduleTeam = {
  team: {
    id: number;
    name: string;
    abbreviation?: string;
  };
  probablePitcher?: {
    id: number;
    fullName: string;
  };
};

type MlbScheduleGame = {
  gamePk: number;
  gameDate: string;
  status?: {
    detailedState?: string;
    statusCode?: string;
  };
  teams: {
    home: MlbScheduleTeam;
    away: MlbScheduleTeam;
  };
  venue?: {
    name?: string;
  };
  weather?: {
    condition?: string;
    temp?: string;
    wind?: string;
  };
};

type MlbScheduleResponse = {
  dates?: Array<{
    games?: MlbScheduleGame[];
  }>;
};

type MlbPeopleResponse = {
  people?: MlbPerson[];
};

type MlbLiveFeedResponse = {
  liveData?: {
    boxscore?: {
      teams?: {
        home?: {
          battingOrder?: Array<number | string>;
        };
        away?: {
          battingOrder?: Array<number | string>;
        };
      };
    };
  };
};

const jsonCache = new Map<string, CacheEntry<unknown>>();

function getCache<T>(key: string): T | null {
  const entry = jsonCache.get(key);
  if (!entry || entry.expiresAt < Date.now()) {
    jsonCache.delete(key);
    return null;
  }
  return entry.value as T;
}

function setCache<T>(key: string, value: T): T {
  jsonCache.set(key, {
    expiresAt: Date.now() + CACHE_TTL_MS,
    value,
  });
  return value;
}

async function fetchJson<T>(url: string): Promise<T> {
  const cached = getCache<T>(url);
  if (cached) {
    return cached;
  }

  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`MLB API request failed: ${response.status} ${response.statusText}`);
  }
  return setCache(url, (await response.json()) as T);
}

function getTodayInChicago(): string {
  return new Intl.DateTimeFormat("en-CA", {
    timeZone: "America/Chicago",
    year: "numeric",
    month: "2-digit",
    day: "2-digit",
  }).format(new Date());
}

function toTeamContext(team: MlbScheduleTeam["team"]): FantasyTeamContext {
  return {
    id: team.id,
    name: team.name,
    abbreviation: team.abbreviation ?? null,
  };
}

function normalizeHand(code?: string | null): string | null {
  if (!code) {
    return null;
  }
  if (code === "R") return "R";
  if (code === "L") return "L";
  if (code === "S") return "S";
  return code;
}

function buildPitcherContext(
  probablePitcher: MlbScheduleTeam["probablePitcher"] | undefined,
  peopleById: Map<number, MlbPerson>
): FantasyPitcherContext | null {
  if (!probablePitcher) {
    return null;
  }

  return {
    id: probablePitcher.id,
    name: probablePitcher.fullName,
    pitchHand: normalizeHand(peopleById.get(probablePitcher.id)?.pitchHand?.code),
  };
}

function buildPlatoonLabel(batSide?: string | null, pitchHand?: string | null): string | null {
  if (!batSide || !pitchHand) {
    return null;
  }
  return `${batSide}HB vs ${pitchHand}HP`;
}

async function fetchSchedule(date: string): Promise<MlbScheduleGame[]> {
  const params = new URLSearchParams({
    sportId: "1",
    date,
    hydrate: "probablePitcher,team,venue,weather",
  });
  const data = await fetchJson<MlbScheduleResponse>(`${MLB_API_BASE}/v1/schedule?${params.toString()}`);
  return data.dates?.flatMap((entry) => entry.games ?? []) ?? [];
}

async function fetchPeople(ids: number[]): Promise<Map<number, MlbPerson>> {
  const uniqueIds = Array.from(new Set(ids.filter((id) => Number.isFinite(id))));
  if (uniqueIds.length === 0) {
    return new Map();
  }

  const params = new URLSearchParams({
    personIds: uniqueIds.join(","),
    hydrate: "currentTeam",
  });
  const data = await fetchJson<MlbPeopleResponse>(`${MLB_API_BASE}/v1/people?${params.toString()}`);
  return new Map((data.people ?? []).map((person) => [person.id, person]));
}

async function fetchLineupStatus(gamePk: number, side: "home" | "away", mlbamid: number): Promise<DailyMatchupContext["lineupStatus"]> {
  const data = await fetchJson<MlbLiveFeedResponse>(`${MLB_API_BASE}/v1.1/game/${gamePk}/feed/live`);
  const order = data.liveData?.boxscore?.teams?.[side]?.battingOrder ?? [];
  if (order.length === 0) {
    return "unavailable";
  }
  return order.map((id) => String(id)).includes(String(mlbamid)) ? "confirmed" : "notInLineup";
}

export async function getDailyMatchupContext({
  player,
  playerType,
  date = getTodayInChicago(),
}: {
  player: HitterRecord | PitcherRecord;
  playerType: PlayerType;
  date?: string;
}): Promise<DailyMatchupContext> {
  const mlbamid = Number(player.mlbamid);
  if (!Number.isFinite(mlbamid) || mlbamid <= 0) {
    return {
      matchupStatus: "missing_mlbam_id",
      date,
    };
  }

  const schedule = await fetchSchedule(date);
  const probablePitcherIds = schedule.flatMap((game) => [
    game.teams.home.probablePitcher?.id,
    game.teams.away.probablePitcher?.id,
  ]).filter((id): id is number => typeof id === "number");
  const peopleById = await fetchPeople([mlbamid, ...probablePitcherIds]);
  const selectedPerson = peopleById.get(mlbamid);
  const selectedTeamId = selectedPerson?.currentTeam?.id;

  if (!selectedTeamId) {
    return {
      matchupStatus: "no_game",
      date,
      selectedPlayerHandedness: {
        batSide: normalizeHand(selectedPerson?.batSide?.code),
        pitchHand: normalizeHand(selectedPerson?.pitchHand?.code),
      },
    };
  }

  const game = schedule.find(
    (entry) => entry.teams.home.team.id === selectedTeamId || entry.teams.away.team.id === selectedTeamId
  );
  if (!game) {
    return {
      matchupStatus: "no_game",
      date,
      playerTeam: selectedPerson?.currentTeam ? { id: selectedPerson.currentTeam.id, name: selectedPerson.currentTeam.name } : undefined,
      selectedPlayerHandedness: {
        batSide: normalizeHand(selectedPerson?.batSide?.code),
        pitchHand: normalizeHand(selectedPerson?.pitchHand?.code),
      },
    };
  }

  const homeAway = game.teams.home.team.id === selectedTeamId ? "home" : "away";
  const selectedSide = homeAway === "home" ? game.teams.home : game.teams.away;
  const opponentSide = homeAway === "home" ? game.teams.away : game.teams.home;
  const selectedTeamStarter = buildPitcherContext(selectedSide.probablePitcher, peopleById);
  const opposingStarter = buildPitcherContext(opponentSide.probablePitcher, peopleById);
  const batSide = normalizeHand(selectedPerson?.batSide?.code);
  const pitchHand = normalizeHand(selectedPerson?.pitchHand?.code);
  const isPostponed = game.status?.statusCode === "DR" || game.status?.detailedState === "Postponed";

  return {
    matchupStatus: "ok",
    date,
    gamePk: game.gamePk,
    gameDate: game.gameDate,
    gameStatus: game.status?.detailedState,
    isPostponed,
    homeAway,
    playerTeam: toTeamContext(selectedSide.team),
    opponent: toTeamContext(opponentSide.team),
    venue: game.venue?.name ?? null,
    weather: game.weather
      ? {
          condition: game.weather.condition ?? null,
          temp: game.weather.temp ?? null,
          wind: game.weather.wind ?? null,
        }
      : null,
    selectedPlayerHandedness: {
      batSide,
      pitchHand,
    },
    opposingStarter,
    selectedTeamStarter,
    isProbableStarter: playerType === "pitcher" ? selectedSide.probablePitcher?.id === mlbamid : undefined,
    platoonLabel: playerType === "hitter" ? buildPlatoonLabel(batSide, opposingStarter?.pitchHand) : null,
    lineupStatus: playerType === "hitter" ? await fetchLineupStatus(game.gamePk, homeAway, mlbamid) : undefined,
  };
}

export function __clearMlbMatchupCacheForTests(): void {
  jsonCache.clear();
}
