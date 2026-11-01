const VOTER_ID_STORAGE_KEY = "mcfarland-voter-id-v1";

// Used when browser storage is blocked, so one browser tab still votes as one voter.
let sessionVoterId: string | null = null;

function generateId(): string {
  if (typeof crypto !== "undefined" && "randomUUID" in crypto) {
    return crypto.randomUUID();
  }
  return `${Date.now()}-${Math.random().toString(36).slice(2)}-${Math.random().toString(36).slice(2)}`;
}

/**
 * A random id that lets the server count one vote per browser. It is kept separate from the analytics
 * session id, so votes cannot be linked to anything else a visitor does.
 */
export function getOrCreateVoterId(): string {
  if (sessionVoterId) {
    return sessionVoterId;
  }

  try {
    const stored = window.localStorage.getItem(VOTER_ID_STORAGE_KEY);
    if (stored && stored.length >= 16) {
      sessionVoterId = stored;
      return stored;
    }
  } catch {
    // Storage is blocked; fall through to a session-only id.
  }

  const created = generateId();
  sessionVoterId = created;
  try {
    window.localStorage.setItem(VOTER_ID_STORAGE_KEY, created);
  } catch {
    // Keep the session-only id.
  }
  return created;
}

export function __resetVoterIdForTests(): void {
  sessionVoterId = null;
}
