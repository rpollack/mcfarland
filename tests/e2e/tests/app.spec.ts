import { expect, test } from "@playwright/test";

test.describe("McFarland experience", () => {
  test("supports single player and comparison flows", async ({ page }) => {
    await page.goto("/");

    await expect(page.getByRole("heading", { name: "McFarland" })).toBeVisible();
    await expect(page.getByLabel("Search by player name")).toBeVisible();

    await expect(page.locator('section[aria-label="Player statistics"] h2')).toBeVisible({ timeout: 15000 });
    await expect(page.getByText("Quick Insight", { exact: false })).toBeVisible();

    await page.getByRole("link", { name: "Compare Players" }).click();

    await page.getByLabel("Player A search").fill("Judge");
    await page.waitForSelector("#player-a-select >> text=Aaron Judge");
    await page.selectOption("#player-a-select", { label: /Aaron Judge/ });

    await page.getByLabel("Player B search").fill("Soto");
    await page.waitForSelector("#player-b-select >> text=Juan Soto");
    await page.selectOption("#player-b-select", { label: /Juan Soto/ });

    await expect(page.getByRole("table")).toBeVisible({ timeout: 15000 });
    await expect(page.getByText("Recommended", { exact: false })).toBeVisible();

    await page.getByRole("link", { name: "About" }).click();
    await expect(page.getByRole("heading", { level: 2, name: /McFarland/ })).toBeVisible();
    await expect(page.getByText("Node.js API", { exact: false })).toBeVisible();
  });
});
