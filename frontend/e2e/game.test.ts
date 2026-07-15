import { test, expect, type Page, type Locator } from "@playwright/test";

/**
 * Pieces are rendered as flat siblings of the squares grid, positioned by
 * CSS transform; each piece-slot exposes its current square via the
 * `data-square` attribute. Captured pieces stay in the DOM briefly while
 * their exit animation plays — we exclude `.exiting` so assertions reflect
 * the model state, not animation residue.
 */
function pieceAt(page: Page, square: number): Locator {
  return page.locator(
    `.piece-slot[data-square="${square}"]:not(.exiting) .piece`,
  );
}

/** All currently-live pieces on the board. */
function livePieces(page: Page): Locator {
  return page.locator(".piece-slot:not(.exiting) .piece");
}

async function startHotseatGame(page: Page) {
  await page.goto("/");
  // "Hotseat" is the second entry in .entries
  await page.locator(".entries button").nth(1).click();
  await page.getByRole("button", { name: "Begin game" }).click();
  await expect(page.locator(".board")).toBeVisible();
  // Wait for the initial game state to load and pieces to render so any
  // immediate piece-count assertions race against fully-populated DOM.
  await expect(livePieces(page)).toHaveCount(37);
}

async function makeMove(page: Page, from: number, to: number) {
  await page.locator(`[data-index="${from}"]`).click();
  await expect(page.locator(`[data-index="${to}"].valid-move`)).toBeVisible();
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
    await expect(page.locator(".entries button")).toHaveCount(3);
    await expect(page.locator(".entries button .title").nth(0)).toHaveText(
      "Against AI",
    );
    await expect(page.locator(".entries button .title").nth(1)).toHaveText(
      "Hotseat",
    );
    await expect(page.locator(".entries button .title").nth(2)).toHaveText(
      "Online",
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
    await expect(livePieces(page)).toHaveCount(37);
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
    await expect(pieceAt(page, 2)).toBeVisible();
    await expect(pieceAt(page, 3)).toHaveCount(0);
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
    await desktop(page)
      .locator(".game-actions button", { hasText: "Undo" })
      .click();
    await expect(desktop(page).locator(".game-status")).toContainText(
      "Black to move",
    );
    await expect(pieceAt(page, 3)).toBeVisible();
    await expect(pieceAt(page, 2)).toHaveCount(0);
  });

  test("multiple turns of play maintain correct piece count", async ({
    page,
  }) => {
    await startHotseatGame(page);
    const pieceCountBefore = await livePieces(page).count();
    await makeMove(page, 7, 18);
    await expect(desktop(page).locator(".game-status")).toContainText(
      "White to move",
    );
    const pieceCountAfter = await livePieces(page).count();
    expect(pieceCountAfter).toBe(pieceCountBefore);
  });

  test("new game button returns to home page", async ({ page }) => {
    await startHotseatGame(page);
    await desktop(page)
      .locator(".game-actions button", { hasText: "New" })
      .click();
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

    const navBar = desktop(page).locator(".moves-nav");
    await navBar.locator("button").nth(1).click();

    // 3rd move (4→3) undone: piece at 3 should be gone, piece at 4 restored
    await expect(pieceAt(page, 3)).toHaveCount(0);
    await expect(pieceAt(page, 4)).toBeVisible();
  });

  test("forward button restores the current board state", async ({ page }) => {
    await startAndMakeMoves(page);
    const navBar = desktop(page).locator(".moves-nav");
    const prevBtn = navBar.locator("button").nth(1);
    const nextBtn = navBar.locator("button").nth(2);

    await prevBtn.click();
    await expect(pieceAt(page, 3)).toHaveCount(0);

    await nextBtn.click();
    await expect(pieceAt(page, 3)).toBeVisible();
  });
});

test.describe("Online timeout", () => {
  async function startOnlineGame(page: Page) {
    await page.goto("/");
    await page.locator(".entries button").nth(2).click();
    // Select a time control (first timed option)
    await page.getByText("5 min", { exact: true }).click();
    await page.getByRole("button", { name: "Create game" }).click();
    await page.getByRole("button", { name: "Continue to game" }).click();
    await expect(page.locator(".board")).toBeVisible();
    await expect(livePieces(page)).toHaveCount(37);
  }

  test("timeout displays correct game outcome", async ({ page }) => {
    await startOnlineGame(page);
    await page.evaluate(() => window.__simulateTimeout?.(100));
    await expect(desktop(page).locator(".game-status")).toContainText(
      "timeout",
      { timeout: 2000 },
    );
  });
});
