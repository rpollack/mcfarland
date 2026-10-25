import { useMemo, useState } from "react";
import { useQuery } from "@tanstack/react-query";
import { fetchVibes } from "../api";
import { VibeContext } from "./vibe";

export function VibeProvider({
  children,
  initialMode,
}: {
  children: React.ReactNode;
  initialMode?: string | null;
}) {
  const { data, isLoading } = useQuery({
    queryKey: ["vibes"],
    queryFn: fetchVibes,
  });

  const defaultMode = data?.defaultMode ?? "straightforward";
  // null means "no explicit choice yet", so the server default applies once it loads.
  const [selectedMode, setSelectedMode] = useState<string | null>(initialMode ?? null);
  const [syncedInitialMode, setSyncedInitialMode] = useState<string | null>(initialMode ?? null);
  const mode = selectedMode ?? defaultMode;

  if ((initialMode ?? null) !== syncedInitialMode) {
    setSyncedInitialMode(initialMode ?? null);
    if (initialMode && initialMode !== mode) {
      setSelectedMode(initialMode);
    }
  }

  const value = useMemo(
    () => ({
      vibes: data?.vibes ?? [],
      mode,
      setMode: setSelectedMode,
      isLoading,
      defaultMode,
    }),
    [data, mode, isLoading, defaultMode]
  );

  return <VibeContext.Provider value={value}>{children}</VibeContext.Provider>;
}
