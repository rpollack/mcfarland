import React from "react";
import ReactDOM from "react-dom/client";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import App from "./App";
import "./styles/global.css";
import { BrowserRouter, useLocation } from "react-router-dom";
import { getOrCreateSessionId } from "./api";

const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      refetchOnWindowFocus: false,
    },
  },
});

function AmplitudePageTracker() {
  const location = useLocation();
  const lastTrackedPathRef = React.useRef<string | null>(null);
  const sessionIdRef = React.useRef<string | null>(null);

  if (sessionIdRef.current === null) {
    sessionIdRef.current = getOrCreateSessionId();
  }

  React.useEffect(() => {
    if (!sessionIdRef.current) {
      return;
    }
    window.amplitude?.setUserId?.(sessionIdRef.current);
  }, []);

  React.useEffect(() => {
    const path = `${location.pathname}${location.search}${location.hash}`;
    if (lastTrackedPathRef.current === path) {
      return;
    }
    lastTrackedPathRef.current = path;
    const eventProperties: Record<string, unknown> = { path };
    if (sessionIdRef.current) {
      eventProperties.session_id = sessionIdRef.current;
    }
    window.amplitude?.track("page_view", eventProperties);
  }, [location.pathname, location.search, location.hash]);

  return null;
}

ReactDOM.createRoot(document.getElementById("root")!).render(
  <React.StrictMode>
    <QueryClientProvider client={queryClient}>
      <BrowserRouter>
        <AmplitudePageTracker />
        <App />
      </BrowserRouter>
    </QueryClientProvider>
  </React.StrictMode>
);
