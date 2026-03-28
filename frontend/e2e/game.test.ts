import { test, expect, type Page } from "@playwright/test";

async function startHotseatGame(page: Page) {
  await page.goto("/");
  await page.locator(".action-card").first().click();
  await page.locator(".setup-start-btn").click();
  await expect(page.locator(".board")).toBeVisible();
}

async function makeMove(page: Page, from: number, to: number) {
  await page.locator(`[data-index="${from}"]`).click();
  await expect(
    page.locator(`[data-index="${to}"].valid-move`),
  ).toBeVisible();
  await page.locator(`[data-index="${to}"]`).click();
}

/** Scope queries to the desktop right column to avoid mobile duplicates */
function desktop(page: Page) {
  return page.locator(".desktop-only");
}

test.describe("Navigation", () => {
  test("home page shows game mode options", async ({ page }) => {
    await page.goto("/");
    await expect(page.locator("h1")).toHaveText("Hnefatafl");
    await expect(page.locator(".action-card")).toHaveCount(3);
    await expect(page.locator(".action-label").nth(0)).toHaveText(
      "Play Hotseat",
    );
    await expect(page.locator(".action-label").nth(1)).toHaveText(
      "Play vs AI",
    );
    await expect(page.locator(".action-label").nth(2)).toHaveText(
      "Play Online",
    );
  });

  test("settings page is reachable via gear icon", async ({ page }) => {
    await page.goto("/");
    await page.locator('a[href="/settings"]').click();
    await expect(page.locator(".settings-title")).toHaveText("Settings");
    await expect(page.locator(".settings-group")).toHaveCount(3);
  });
});

test.describe("Hotseat game flow", () => {
  test("starting a hotseat game shows board with all pieces", async ({
    page,
  }) => {
    await startHotseatGame(page);
    await expect(page.locator("[data-index]")).toHaveCount(121);
    await expect(page.locator(".piece")).toHaveCount(37);
  });

  test("status shows 'Black to move' at game start", async ({ page }) => {
    await startHotseatGame(page);
    await expect(desktop(page).locator(".game-status")).toContainText(
      "Black to move",
    );
  });

  test("clicking a black piece shows valid move highlights", async ({
    page,
  }) => {
    await startHotseatGame(page);
    await page.locator('[data-index="3"]').click();
    await expect(page.locator(".valid-move").first()).toBeVisible();
  });

  test("clicking an empty square clears highlights", async ({ page }) => {
    await startHotseatGame(page);
    await page.locator('[data-index="3"]').click();
    await expect(page.locator(".valid-move").first()).toBeVisible();
    await page.locator('[data-index="30"]').click();
    await expect(page.locator(".valid-move")).toHaveCount(0);
  });

  test("making a move updates the board and switches turn", async ({
    page,
  }) => {
    await startHotseatGame(page);
    await makeMove(page, 3, 2);
    await expect(desktop(page).locator(".game-status")).toContainText(
      "White to move",
    );
    await expect(page.locator('[data-index="2"] .piece')).toBeVisible();
    await expect(page.locator('[data-index="3"] .piece')).toHaveCount(0);
  });

  test("move appears in history", async ({ page }) => {
    await startHotseatGame(page);
    await makeMove(page, 3, 2);
    await expect(desktop(page).locator(".move-row")).toHaveCount(1);
  });

  test("undo reverts the last move", async ({ page }) => {
    await startHotseatGame(page);
    await makeMove(page, 3, 2);
    await expect(desktop(page).locator(".game-status")).toContainText(
      "White to move",
    );
    await desktop(page).getByRole("button", { name: "Undo" }).click();
    await expect(desktop(page).locator(".game-status")).toContainText(
      "Black to move",
    );
    await expect(page.locator('[data-index="3"] .piece')).toBeVisible();
    await expect(page.locator('[data-index="2"] .piece')).toHaveCount(0);
  });

  test("multiple turns of play maintain correct piece count", async ({
    page,
  }) => {
    await startHotseatGame(page);
    const pieceCountBefore = await page.locator(".piece").count();
    await makeMove(page, 7, 18);
    await expect(desktop(page).locator(".game-status")).toContainText(
      "White to move",
    );
    const pieceCountAfter = await page.locator(".piece").count();
    expect(pieceCountAfter).toBe(pieceCountBefore);
  });

  test("new game button returns to home page", async ({ page }) => {
    await startHotseatGame(page);
    await desktop(page).getByRole("button", { name: "New Game" }).click();
    await expect(page.locator("h1")).toHaveText("Hnefatafl");
  });
});

test.describe("History navigation", () => {
  async function startAndMakeMoves(page: Page) {
    await startHotseatGame(page);
    // Black: d11 (3) -> c11 (2)
    await makeMove(page, 3, 2);
    // White: f8 (38) -> f9 (27)
    await makeMove(page, 38, 27);
    // Black: e11 (4) -> d11 (3)
    await makeMove(page, 4, 3);
  }

  test("back button shows a previous board state", async ({ page }) => {
    await startAndMakeMoves(page);
    await expect(desktop(page).locator(".game-status")).toContainText(
      "White to move",
    );

    const navToolbar = desktop(page).locator(
      '[aria-label="Move navigation"]',
    );
    await navToolbar.locator("button").nth(1).click();

    // 3rd move (4→3) undone: piece at 3 should be gone, piece at 4 restored
    await expect(page.locator('[data-index="3"] .piece')).toHaveCount(0);
    await expect(page.locator('[data-index="4"] .piece')).toBeVisible();
  });

  test("forward button restores the current board state", async ({ page }) => {
    await startAndMakeMoves(page);
    const navToolbar = desktop(page).locator(
      '[aria-label="Move navigation"]',
    );
    const prevBtn = navToolbar.locator("button").nth(1);
    const nextBtn = navToolbar.locator("button").nth(2);

    await prevBtn.click();
    await expect(page.locator('[data-index="3"] .piece')).toHaveCount(0);

    await nextBtn.click();
    await expect(page.locator('[data-index="3"] .piece')).toBeVisible();
  });
});
