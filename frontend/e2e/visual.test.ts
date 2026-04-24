import { test, expect, type Page } from "@playwright/test";

async function startHotseatGame(page: Page) {
  await page.goto("/");
  // "Hotseat" is the second entry in .entries
  await page.locator(".entries a").nth(1).click();
  await page.getByRole("button", { name: "Begin game" }).click();
  await expect(page.locator(".board")).toBeVisible();
}

async function makeMove(page: Page, from: number, to: number) {
  await page.locator(`[data-index="${from}"]`).click();
  await expect(
    page.locator(`[data-index="${to}"].valid-move`),
  ).toBeVisible();
  await page.locator(`[data-index="${to}"]`).click();
}

test.describe("Visual regression", () => {
  test("home page", async ({ page }) => {
    await page.goto("/");
    await expect(page.locator("h1")).toBeVisible();
    await expect(page).toHaveScreenshot("home.png");
  });

  test("hotseat setup modal", async ({ page }) => {
    await page.goto("/");
    await page.locator(".entries a").nth(1).click();
    await expect(
      page.getByRole("button", { name: "Begin game" }),
    ).toBeVisible();
    await expect(page).toHaveScreenshot("hotseat-setup-modal.png");
  });

  test("ai setup modal", async ({ page }) => {
    await page.goto("/");
    await page.locator(".entries a").nth(0).click();
    await expect(
      page.getByRole("button", { name: "Begin game" }),
    ).toBeVisible();
    await expect(page).toHaveScreenshot("ai-setup-modal.png");
  });

  test("game board initial state", async ({ page }) => {
    await startHotseatGame(page);
    await expect(page).toHaveScreenshot("game-board-initial.png");
  });

  test("game board with piece selected", async ({ page }) => {
    await startHotseatGame(page);
    await page.locator('[data-index="3"]').click();
    await expect(page.locator(".valid-move").first()).toBeVisible();
    await expect(page).toHaveScreenshot("game-board-piece-selected.png");
  });

  test("game board after a move", async ({ page }) => {
    await startHotseatGame(page);
    await makeMove(page, 3, 2);
    await expect(
      page.locator(".desktop-only .game-status"),
    ).toContainText("White to move");
    await expect(page).toHaveScreenshot("game-board-after-move.png");
  });

  test("settings page", async ({ page }) => {
    await page.goto("/settings");
    await expect(page.locator(".settings-title")).toHaveText("Settings");
    await expect(page).toHaveScreenshot("settings.png");
  });
});
