import { expect, test, type Page, type Request } from "@playwright/test";

async function waitForPostRequestFinished(
  page: Page,
  pathSuffix: string,
  predicate?: (request: Request) => boolean
) {
  await page.waitForEvent("requestfinished", (request) => {
    if (request.method() !== "POST" || !request.url().endsWith(pathSuffix)) {
      return false;
    }
    return predicate ? predicate(request) : true;
  });
}

async function expectAnalysisReady(page: Page) {
  const panel = page.locator("section[aria-label='AI analysis']");
  const article = panel.locator("article[aria-live='polite']");
  const vibeSelect = page.locator("#vibe-select");

  await expect(panel).toBeVisible({ timeout: 15000 });
  await expect(article).toBeVisible({ timeout: 15000 });
  await expect(article).not.toHaveText(/^\s*$/, { timeout: 15000 });
  await expect(vibeSelect).toBeVisible({ timeout: 15000 });
}

test.describe("McFARLAND core experience", () => {
  test("single analysis, vibe switch, compare three players, share link", async ({ page, context }) => {
    const singleAnalyzeRequests: { mode: string; playerId: string }[] = [];
    const compareAnalyzeRequests: { mode: string; playerIds: string[] }[] = [];

    const recordApiEvents = (targetPage: typeof page) => {
      targetPage.on("request", (request) => {
        if (request.method() !== "POST") {
          return;
        }
        const url = request.url();
        if (url.endsWith("/api/analyze")) {
          try {
            const body = JSON.parse(request.postData() ?? "{}");
            singleAnalyzeRequests.push({
              mode: String(body.analysisMode ?? ""),
              playerId: String(body.playerId ?? ""),
            });
          } catch {
            /* ignore */
          }
        } else if (url.endsWith("/api/compare/analyze")) {
          try {
            const body = JSON.parse(request.postData() ?? "{}");
            compareAnalyzeRequests.push({
              mode: String(body.analysisMode ?? ""),
              playerIds: Array.isArray(body.playerIds) ? body.playerIds.map(String) : [],
            });
          } catch {
            /* ignore */
          }
        }
      });
    };

    recordApiEvents(page);

    await page.goto("/");
    await expect(page.getByRole("heading", { name: "McFARLAND" })).toBeVisible();

    const singleSearch = page.getByPlaceholder("Find hitters...");
    const initialAnalyzeFinished = waitForPostRequestFinished(page, "/api/analyze");
    await singleSearch.fill("Aaron Judge");
    await page.getByRole("button", { name: /^Aaron Judge$/i }).first().click();
    await initialAnalyzeFinished;
    await expectAnalysisReady(page);

    const straightforwardRuns = singleAnalyzeRequests.filter((entry) => entry.mode === "straightforward");
    expect(straightforwardRuns.length).toBeGreaterThan(0);
    expect(straightforwardRuns.at(-1)?.playerId).not.toBe("");

    const vibeSelect = page.locator("#vibe-select");
    const vibeSwitchFinished = waitForPostRequestFinished(
      page,
      "/api/analyze",
      (request) => (request.postData() ?? "").includes("analytics_dork")
    );
    await vibeSelect.selectOption("analytics_dork");
    await vibeSwitchFinished;
    await expect(vibeSelect).toHaveValue("analytics_dork");
    await expectAnalysisReady(page);

    const analyticsRuns = singleAnalyzeRequests.filter((entry) => entry.mode === "analytics_dork");
    expect(analyticsRuns).toHaveLength(1);

    await page.getByRole("tab", { name: "Compare Players" }).click();

    const compareSearch = page.getByPlaceholder("Find hitters...");
    const addComparePlayer = async (fullName: string) => {
      await compareSearch.fill(fullName);
      await page.getByRole("button", { name: new RegExp(`^${fullName}$`, "i") }).first().click();
    };

    await addComparePlayer("Aaron Judge");
    await addComparePlayer("Juan Soto");
    await addComparePlayer("Mike Trout");

    const initialCompareCount = compareAnalyzeRequests.length;
    const compareRequestFinished = waitForPostRequestFinished(page, "/api/compare/analyze");
    await page.getByRole("button", { name: "Compare" }).click();
    await compareRequestFinished;

    await expectAnalysisReady(page);
    await expect(page.locator("#vibe-select")).toHaveValue("analytics_dork");

    expect(compareAnalyzeRequests.length).toBe(initialCompareCount + 1);
    const lastCompare = compareAnalyzeRequests.at(-1);
    expect(lastCompare?.playerIds).toHaveLength(3);
    expect(lastCompare?.mode).toBe("analytics_dork");

    const shareUrl = page.url();
    const sharePage = await context.newPage();
    recordApiEvents(sharePage);
    const sharedCompareRequestFinished = waitForPostRequestFinished(sharePage, "/api/compare/analyze");
    await sharePage.goto(shareUrl);

    await sharedCompareRequestFinished;
    await expectAnalysisReady(sharePage);
    await expect(sharePage.locator("#vibe-select")).toHaveValue("analytics_dork");

    await sharePage.close();
  });
});
