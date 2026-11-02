import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { render, screen, within } from "@testing-library/react";
import { MemoryRouter } from "react-router-dom";
import { afterEach, describe, expect, it, vi } from "vitest";
import AboutPage from "../AboutPage";
import * as api from "../../api";

function renderAbout() {
  vi.spyOn(api, "fetchDataFreshness").mockResolvedValue({ dataThroughDate: "2026-10-30", dataThroughLabel: "October 30" });
  const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
  render(
    <QueryClientProvider client={client}>
      <MemoryRouter>
        <AboutPage />
      </MemoryRouter>
    </QueryClientProvider>
  );
}

describe("AboutPage", () => {
  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("covers each main part of the app", () => {
    renderAbout();

    for (const heading of ["What you'll find", "Reading the luck numbers", "How quick links work", "Why it exists", "Your data", "Pick a vibe, change the story"]) {
      expect(screen.getByRole("heading", { name: heading })).toBeInTheDocument();
    }

    const findings = screen.getByRole("heading", { name: "What you'll find" }).closest("section")!;
    for (const feature of ["Single-player takes", "The mirage board", "Keep going", "Compare mode", "Fantasy Tools", "Vibes", "Stars", "Share cards"]) {
      expect(within(findings).getByText(feature)).toBeInTheDocument();
    }
  });

  it("explains how to read the luck gap for hitters and pitchers", () => {
    renderAbout();

    const luck = screen.getByRole("heading", { name: "Reading the luck numbers" }).closest("section")!;
    expect(within(luck).getByText("xwOBA")).toBeInTheDocument();
    expect(within(luck).getByText("xERA")).toBeInTheDocument();
    expect(within(luck).getByText("luck gap")).toBeInTheDocument();
  });

  it("lists all nine vibes", () => {
    renderAbout();

    const vibes = screen.getByRole("heading", { name: "Pick a vibe, change the story" }).closest("section")!;
    expect(within(vibes).getAllByRole("listitem")).toHaveLength(9);
  });

  it("shows how current the data is", async () => {
    renderAbout();

    expect(await screen.findByText("Data is current through games on October 30.")).toBeInTheDocument();
  });

  it("lists past announcements under What's new", () => {
    renderAbout();

    expect(screen.getByRole("heading", { name: "What's new" })).toBeInTheDocument();
  });
});
