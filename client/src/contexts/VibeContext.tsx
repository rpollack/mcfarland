import { createContext, useContext, useEffect, useMemo, useRef, useState } from "react";
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

  const fallbackMode = "straightforward";
  const defaultMode = data?.defaultMode ?? fallbackMode;
  const [mode, setModeState] = useState<string>(() => initialMode ?? defaultMode);
  const prevInitialModeRef = useRef<string | null>(initialMode ?? null);

  useEffect(() => {
    if (!initialMode && mode === fallbackMode && defaultMode !== fallbackMode) {
      setModeState(defaultMode);
    }
  }, [defaultMode, fallbackMode, initialMode, mode]);

  useEffect(() => {
    if (initialMode === prevInitialModeRef.current) {
      return;
    }
    prevInitialModeRef.current = initialMode ?? null;
    if (initialMode && initialMode !== mode) {
      setModeState(initialMode);
    }
  }, [initialMode, mode]);

  const value = useMemo(
    () => ({
      vibes: data?.vibes ?? [],
      mode,
      setMode: setModeState,
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
