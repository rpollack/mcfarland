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
  test("analyze again reselects player and updates analysis headshot", async ({ page }) => {
    const singleAnalyzeRequests: { mode: string; playerId: string }[] = [];

    page.on("request", (request) => {
      if (request.method() !== "POST" || !request.url().endsWith("/api/analyze")) {
        return;
      }
      try {
        const body = JSON.parse(request.postData() ?? "{}");
        singleAnalyzeRequests.push({
          mode: String(body.analysisMode ?? ""),
          playerId: String(body.playerId ?? ""),
        });
      } catch {
        /* ignore */
      }
    });

    await page.goto("/");
    await page.evaluate(() => {
      window.localStorage.clear();
    });
    await page.reload();
    await expect(page.getByRole("heading", { name: "McFARLAND" })).toBeVisible();

    const onFireRow = page.locator("section[aria-label='On Fire']");
    const iceColdRow = page.locator("section[aria-label='Ice Cold']");
    const analyzeAgainRow = page.locator("section[aria-label='Analyze again']");
    const analysisPanel = page.locator("section[aria-label='AI analysis']");
    const analysisHeadshot = analysisPanel.locator("img[alt$='headshot']").first();

    await expect(onFireRow).toBeVisible();
    await expect(iceColdRow).toBeVisible();

    const onFireButton = onFireRow.locator("button").first();
    const firstPlayerName = ((await onFireButton.textContent()) ?? "").trim();
    expect(firstPlayerName).not.toBe("");

    const firstAnalyzeFinished = waitForPostRequestFinished(page, "/api/analyze");
    await onFireButton.click();
    await firstAnalyzeFinished;
    await expectAnalysisReady(page);

    const firstPlayerRequest = singleAnalyzeRequests.at(-1);
    expect(firstPlayerRequest?.playerId).toBeTruthy();
    await expect(analysisPanel.locator("h2")).toContainText(firstPlayerName);
    const firstHeadshotSrc = await analysisHeadshot.getAttribute("src");
    expect(firstHeadshotSrc).toBeTruthy();

    const onFireButtons = onFireRow.locator("button");
    const secondCandidateButton = (await onFireButtons.count()) > 1 ? onFireButtons.nth(1) : iceColdRow.locator("button").first();
    const secondPlayerName = ((await secondCandidateButton.textContent()) ?? "").trim();
    expect(secondPlayerName).not.toBe("");
    expect(secondPlayerName).not.toBe(firstPlayerName);

    const secondAnalyzeFinished = waitForPostRequestFinished(page, "/api/analyze");
    await secondCandidateButton.click();
    await secondAnalyzeFinished;
    await expectAnalysisReady(page);

    const secondPlayerRequest = singleAnalyzeRequests.at(-1);
    expect(secondPlayerRequest?.playerId).toBeTruthy();
    await expect(analysisPanel.locator("h2")).toContainText(secondPlayerName);
    const secondHeadshotSrc = await analysisHeadshot.getAttribute("src");
    expect(secondHeadshotSrc).toBeTruthy();
    expect(secondHeadshotSrc).not.toBe(firstHeadshotSrc);

    await expect(analyzeAgainRow).toBeVisible();
    const analyzeAgainButtons = analyzeAgainRow.locator("button");
    await expect(analyzeAgainButtons).toHaveCount(2, { timeout: 15000 });
    await expect(analyzeAgainButtons.first()).not.toHaveText(/^\d+$/);

    const analyzeAgainFirstPlayerButton = analyzeAgainRow
      .getByRole("button", { name: new RegExp(firstPlayerName, "i") })
      .first();
    await expect(analyzeAgainFirstPlayerButton).toBeVisible();

    const analyzeAgainFinished = waitForPostRequestFinished(
      page,
      "/api/analyze",
      (request) => (request.postData() ?? "").includes(firstPlayerRequest?.playerId ?? "")
    );
    await analyzeAgainFirstPlayerButton.click();
    await analyzeAgainFinished;
    await expectAnalysisReady(page);

    await expect(analysisPanel.locator("h2")).toContainText(firstPlayerName);
    const headshotAfterAnalyzeAgain = await analysisHeadshot.getAttribute("src");
    expect(headshotAfterAnalyzeAgain).toBeTruthy();
    expect(headshotAfterAnalyzeAgain).not.toBe(secondHeadshotSrc);
  });

  test("single-player quick links trigger matching analysis and stay out of compare mode", async ({ page }) => {
    const singleAnalyzeRequests: { mode: string; playerId: string }[] = [];

    page.on("request", (request) => {
      if (request.method() !== "POST" || !request.url().endsWith("/api/analyze")) {
        return;
      }
      try {
        const body = JSON.parse(request.postData() ?? "{}");
        singleAnalyzeRequests.push({
          mode: String(body.analysisMode ?? ""),
          playerId: String(body.playerId ?? ""),
        });
      } catch {
        /* ignore */
      }
    });

    await page.goto("/");
    await expect(page.getByRole("heading", { name: "McFARLAND" })).toBeVisible();

    const onFireRow = page.locator("section[aria-label='On Fire']");
    const iceColdRow = page.locator("section[aria-label='Ice Cold']");
    await expect(onFireRow).toBeVisible();
    await expect(iceColdRow).toBeVisible();

    const firstQuickLink = onFireRow.locator("button").first();
    const clickedPlayerName = (await firstQuickLink.textContent())?.trim() ?? "";
    expect(clickedPlayerName).not.toBe("");

    const analyzeFromQuickLinkFinished = waitForPostRequestFinished(page, "/api/analyze");
    await firstQuickLink.click();
    await analyzeFromQuickLinkFinished;
    await expectAnalysisReady(page);

    await expect(page.locator("h2").first()).toHaveText(new RegExp(clickedPlayerName, "i"));
    expect(singleAnalyzeRequests.length).toBeGreaterThan(0);
    expect(singleAnalyzeRequests.at(-1)?.playerId).not.toBe("");

    await page.getByRole("tab", { name: "Compare Players" }).click();
    await expect(page.locator("section[aria-label='On Fire']")).toHaveCount(0);
    await expect(page.locator("section[aria-label='Ice Cold']")).toHaveCount(0);
  });

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
