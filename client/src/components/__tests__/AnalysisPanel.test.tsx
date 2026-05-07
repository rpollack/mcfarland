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
});
