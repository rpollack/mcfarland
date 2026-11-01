import { fireEvent, render, screen } from "@testing-library/react";
import { MemoryRouter } from "react-router-dom";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import AnnouncementBanner from "../AnnouncementBanner";
import type { Announcement } from "../../announcements";
import { __resetSessionDismissedForTests } from "../../utils/announcements";
import * as api from "../../api";

const announcement: Announcement = {
  id: "test-feature",
  lead: "New:",
  text: "Try the thing.",
  link: { label: "See it", to: "/?mode=compare" },
  startsAt: "2026-11-01",
  endsAt: "2026-11-30",
};
const now = new Date("2026-11-15T12:00:00Z");

function renderBanner(list: Announcement[] = [announcement]) {
  return render(
    <MemoryRouter>
      <AnnouncementBanner announcements={list} now={now} />
    </MemoryRouter>
  );
}

describe("AnnouncementBanner", () => {
  let logSpy: ReturnType<typeof vi.spyOn>;

  beforeEach(() => {
    window.localStorage.clear();
    __resetSessionDismissedForTests();
    logSpy = vi.spyOn(api, "logAnnouncementEvent").mockResolvedValue();
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("shows the active announcement and logs a single view", () => {
    renderBanner();
    expect(screen.getByRole("status")).toHaveTextContent("Try the thing.");
    expect(screen.getByRole("link", { name: "See it" })).toHaveAttribute("href", "/?mode=compare");
    expect(logSpy).toHaveBeenCalledTimes(1);
    expect(logSpy).toHaveBeenCalledWith("test-feature", "viewed");
  });

  it("dismisses on one click and stays dismissed on the next load", () => {
    const { unmount } = renderBanner();
    fireEvent.click(screen.getByRole("button", { name: "Dismiss announcement" }));
    expect(screen.queryByRole("status")).not.toBeInTheDocument();
    expect(logSpy).toHaveBeenCalledWith("test-feature", "dismissed");

    unmount();
    renderBanner();
    expect(screen.queryByRole("status")).not.toBeInTheDocument();
  });

  it("treats following the link as dismissing the announcement", () => {
    renderBanner();
    fireEvent.click(screen.getByRole("link", { name: "See it" }));
    expect(screen.queryByRole("status")).not.toBeInTheDocument();
    expect(logSpy).toHaveBeenCalledWith("test-feature", "clicked");
  });

  it("renders nothing and logs nothing when there is no active announcement", () => {
    renderBanner([{ ...announcement, endsAt: "2026-11-02" }]);
    expect(screen.queryByRole("status")).not.toBeInTheDocument();
    expect(logSpy).not.toHaveBeenCalled();
  });
});
