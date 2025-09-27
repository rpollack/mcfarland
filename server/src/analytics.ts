export async function logSessionStart(sessionId: string): Promise<void> {
  if (!sessionId) {
    return;
  }

  const timestamp = new Date().toISOString();
  console.info(`[analytics] session started`, { sessionId, timestamp });
}
