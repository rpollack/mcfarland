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
  const vibeButton = page.getByRole("button", { name: "Change the vibe" }).first();

  await expect(panel).toBeVisible({ timeout: 15000 });
  await expect(article).toBeVisible({ timeout: 15000 });
  await expect(article).not.toHaveText(/^\s*$/, { timeout: 15000 });
  await expect(vibeButton).toBeVisible({ timeout: 15000 });
}

function singleSearchInput(page: Page) {
  return page.locator("section[aria-label='Player search'] input[type='search']");
}

function compareSearchInput(page: Page) {
  return page.locator("section[aria-label='Comparison search'] input[type='search']");
}

function fantasySearchInput(page: Page) {
  return page.locator("section[aria-label='Player search'] input[type='search']");
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
    await expect(singleSearchInput(page)).toBeVisible();

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
    await expect(singleSearchInput(page)).toBeVisible();

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
    await expect(singleSearchInput(page)).toBeVisible();

    const singleSearch = singleSearchInput(page);
    const initialAnalyzeFinished = waitForPostRequestFinished(page, "/api/analyze");
    await singleSearch.fill("Aaron Judge");
    await page.getByRole("button", { name: /^Aaron Judge$/i }).first().click();
    await initialAnalyzeFinished;
    await expectAnalysisReady(page);

    const straightforwardRuns = singleAnalyzeRequests.filter((entry) => entry.mode === "straightforward");
    expect(straightforwardRuns.length).toBeGreaterThan(0);
    expect(straightforwardRuns.at(-1)?.playerId).not.toBe("");

    const vibeSwitchFinished = waitForPostRequestFinished(
      page,
      "/api/analyze",
      (request) => (request.postData() ?? "").includes("analytics_dork")
    );
    await page.getByRole("button", { name: "Change the vibe" }).first().click();
    await page.getByRole("button", { name: "Analytics Nerd" }).click();
    await vibeSwitchFinished;
    await expectAnalysisReady(page);

    const analyticsRuns = singleAnalyzeRequests.filter((entry) => entry.mode === "analytics_dork");
    expect(analyticsRuns).toHaveLength(1);

    await page.getByRole("tab", { name: "Compare Players" }).click();

    const compareSearch = compareSearchInput(page);
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

    await sharePage.close();
  });

  test("fantasy daily matchup selects a hitter and renders start sit context", async ({ page }) => {
    const fantasyAnalyzeRequests: { playerId: string; playerType: string; date?: string }[] = [];

    await page.route("**/api/fantasy/daily-matchup/analyze", async (route) => {
      const request = route.request();
      const body = JSON.parse(request.postData() ?? "{}");
      fantasyAnalyzeRequests.push({
        playerId: String(body.playerId ?? ""),
        playerType: String(body.playerType ?? ""),
        date: typeof body.date === "string" ? body.date : undefined,
      });

      await route.fulfill({
        status: 200,
        contentType: "application/json",
        body: JSON.stringify({
          player: {
            Name: "Aaron Judge",
            PlayerId: body.playerId,
            Age: 33,
            mlbamid: "592450",
            AVG_cur: 0.31,
            OBP_cur: 0.42,
            SLG_cur: 0.61,
            K_pct_cur: 24,
            BB_pct_cur: 15,
            Barrel_pct_cur: 18,
            BABIP_cur: 0.31,
            wOBA_cur: 0.43,
            xwOBA_cur: 0.45,
            xwOBA_wOBA_gap_cur: 0.02,
            PA_cur: 120,
            AVG_l3: 0.29,
            OBP_l3: 0.4,
            SLG_l3: 0.58,
            K_pct_l3: 25,
            BB_pct_l3: 14,
            Barrel_pct_l3: 17,
            BABIP_l3: 0.3,
            wOBA_l3: 0.41,
            xwOBA_l3: 0.42,
            xwOBA_wOBA_gap_l3: 0.01,
            PA_l3: 540,
            AVG_diff: 0.02,
            OBP_diff: 0.02,
            SLG_diff: 0.03,
            K_pct_diff: -1,
            BB_pct_diff: 1,
            Barrel_pct_diff: 1,
            BABIP_diff: 0.01,
            wOBA_diff: 0.02,
            xwOBA_diff: 0.03,
            xwOBA_wOBA_gap_diff: 0.01,
          },
          matchup: {
            matchupStatus: "ok",
            date: "2026-04-25",
            gamePk: 824203,
            gameDate: "2026-04-25T23:10:00Z",
            gameStatus: "Scheduled",
            homeAway: "away",
            playerTeam: { id: 147, name: "New York Yankees", abbreviation: "NYY" },
            opponent: { id: 117, name: "Houston Astros", abbreviation: "HOU" },
            venue: "Daikin Park",
            weather: { condition: "Clear", temp: "72", wind: "5 mph" },
            selectedPlayerHandedness: { batSide: "R", pitchHand: "R" },
            opposingStarter: { id: 677960, name: "Ryan Weathers", pitchHand: "L" },
            selectedTeamStarter: { id: 681347, name: "Mike Burrows", pitchHand: "R" },
            isProbableStarter: undefined,
            platoonLabel: "RHB vs LHP",
            lineupStatus: "confirmed",
          },
          decision: "START",
          confidence: "Medium",
          headline: "Start Judge for the platoon edge",
          analysis: "START Judge because the lineup is confirmed and the platoon matchup is favorable.",
          prompt: "prompt",
          cached: false,
        }),
      });
    });

    await page.goto("/");
    await page.getByRole("tab", { name: "Fantasy Tools" }).click();

    await expect(page).toHaveURL(/mode=fantasy/);
    await expect(page.getByRole("heading", { name: /Daily Matchup: .+, .+ \d+(st|nd|rd|th)/ })).toBeVisible();

    const search = fantasySearchInput(page);
    await expect(search).toBeVisible();
    await search.fill("Aaron Judge");
    await page.getByRole("button", { name: /^Aaron Judge$/i }).first().click();

    await expect(page.getByRole("heading", { name: "Aaron Judge" })).toBeVisible({ timeout: 15000 });
    expect(
      await page
        .locator("section[aria-label='Start sit decision'], section[aria-label='Daily matchup']")
        .evaluateAll((sections) => sections.map((section) => section.getAttribute("aria-label")))
    ).toEqual(["Start sit decision", "Daily matchup"]);

    const matchupCard = page.getByRole("region", { name: "Daily matchup", exact: true });
    await expect(matchupCard.getByRole("heading", { name: "Daily matchup: Saturday, April 25th" })).toBeVisible();
    await expect(matchupCard).toContainText("NYY at HOU");
    await expect(matchupCard).toContainText("Ryan Weathers (LHP)");
    await expect(matchupCard).toContainText("✓ Yes");
    await expect(matchupCard).toContainText("RHB vs LHP");
    await expect(matchupCard).toContainText("Confirmed in lineup");

    const decisionCard = page.getByLabel("Start sit decision");
    await expect(decisionCard).toContainText("START");
    await expect(decisionCard).toContainText("Medium confidence");
    await expect(decisionCard).toContainText("Start Judge for the platoon edge");

    expect(fantasyAnalyzeRequests).toHaveLength(1);
    expect(fantasyAnalyzeRequests[0].playerType).toBe("hitter");
    expect(fantasyAnalyzeRequests[0].playerId).not.toBe("");
  });
});
