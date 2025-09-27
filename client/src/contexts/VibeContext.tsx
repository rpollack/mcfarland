import { createContext, useContext, useEffect, useMemo, useState } from "react";
import { useQuery } from "@tanstack/react-query";
import { fetchVibes } from "../api";
import type { Vibe } from "../types";

type VibeContextValue = {
  vibes: Vibe[];
  mode: string;
  setMode: (mode: string) => void;
  isLoading: boolean;
  defaultMode: string;
};

const VibeContext = createContext<VibeContextValue | undefined>(undefined);

export function VibeProvider({ children }: { children: React.ReactNode }) {
  const { data, isLoading } = useQuery({
    queryKey: ["vibes"],
    queryFn: fetchVibes,
  });

  const fallbackMode = "straightforward";
  const defaultMode = data?.defaultMode ?? fallbackMode;
  const [mode, setMode] = useState<string>(defaultMode);

  useEffect(() => {
    setMode((current) => (current === fallbackMode ? defaultMode : current));
  }, [defaultMode]);

  const value = useMemo(
    () => ({
      vibes: data?.vibes ?? [],
      mode,
      setMode,
      isLoading,
      defaultMode,
    }),
    [data, mode, isLoading, defaultMode]
  );

  return <VibeContext.Provider value={value}>{children}</VibeContext.Provider>;
}

export function useVibe() {
  const context = useContext(VibeContext);
  if (!context) {
    throw new Error("useVibe must be used within a VibeProvider");
  }
  return context;
}
