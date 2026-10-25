import { createContext, useContext } from "react";
import type { Vibe } from "../types";

export type VibeContextValue = {
  vibes: Vibe[];
  mode: string;
  setMode: (mode: string) => void;
  isLoading: boolean;
  defaultMode: string;
};

export const VibeContext = createContext<VibeContextValue | undefined>(undefined);

export function useVibe() {
  const context = useContext(VibeContext);
  if (!context) {
    throw new Error("useVibe must be used within a VibeProvider");
  }
  return context;
}
