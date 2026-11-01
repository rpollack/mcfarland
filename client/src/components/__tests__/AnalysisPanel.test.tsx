import { render, screen } from "@testing-library/react";
import { describe, expect, it } from "vitest";
import AnalysisPanel from "../AnalysisPanel";

describe("AnalysisPanel", () => {
  it("renders the headline as the takeaway with stat signals as context", () => {
    render(
      <AnalysisPanel
        quickInsight="Improving with a few real skill gains."
        quickStatSignals={[
          { label: "xwOBA +.041", tone: "positive" },
          { label: "Barrel% +3.2", tone: "positive" },
          { label: "BABIP +.062", tone: "neutral" },
        ]}
        headline="The skills are moving in the right direction"
        analysis="The improved contact quality makes the start more interesting."
      />
    );

    expect(screen.queryByText("Quick insight")).not.toBeInTheDocument();
    expect(screen.getByRole("heading", { name: "The skills are moving in the right direction" })).toBeInTheDocument();
    expect(screen.getByLabelText("Key signals")).toBeInTheDocument();
    expect(screen.getByText("xwOBA +.041")).toBeInTheDocument();
    expect(screen.getByText("Barrel% +3.2")).toBeInTheDocument();
    expect(screen.getByText("BABIP +.062")).toBeInTheDocument();
  });

  it("renders markdown in the analysis once the markdown renderer has loaded", async () => {
    render(<AnalysisPanel headline="Takeaway" analysis="He is **locked in** at the plate." />);

    const emphasized = await screen.findByText("locked in");
    expect(emphasized.tagName).toBe("STRONG");
  });

  it("shows the known stat signals and a skeleton while the analysis loads", () => {
    render(
      <AnalysisPanel
        quickStatSignals={[{ label: "xwOBA +.041", tone: "positive" }]}
        isAnalyzing
        modeLabel="Gen Z"
      />
    );

    expect(screen.getByLabelText("Key signals")).toBeInTheDocument();
    expect(screen.getByText("xwOBA +.041")).toBeInTheDocument();
    expect(screen.getByText("Writing the Gen Z take…")).toBeInTheDocument();
    expect(screen.queryByRole("heading")).not.toBeInTheDocument();
    expect(screen.getByLabelText("AI analysis").querySelector("article")).toHaveAttribute("aria-busy", "true");
  });

  it("drops a trailing parenthetical from the vibe label", () => {
    render(<AnalysisPanel isAnalyzing modeLabel="Straightforward (Default)" />);

    expect(screen.getByText("Writing the Straightforward take…")).toBeInTheDocument();
  });

  it("falls back to the generic loading text without a vibe label", () => {
    render(<AnalysisPanel isAnalyzing />);

    expect(screen.getByText("Generating the latest scouting notes…")).toBeInTheDocument();
  });

  it("replaces the skeleton with the takeaway once the analysis arrives", () => {
    render(<AnalysisPanel headline="Takeaway" analysis="Plain text." quickStatSignals={[{ label: "xwOBA +.041" }]} />);

    expect(screen.queryByText(/Writing the/)).not.toBeInTheDocument();
    expect(screen.getByRole("heading", { name: "Takeaway" })).toBeInTheDocument();
    expect(screen.getByLabelText("AI analysis").querySelector("article")).toHaveAttribute("aria-busy", "false");
  });
});
