import { defineConfig } from "@playwright/test";

export default defineConfig({
  testDir: "./e2e",
  // Fail fast: tests against a local dev server should take well under a
  // second. Anything beyond ~3s indicates a real problem, not slowness.
  timeout: 3_000,
  expect: {
    timeout: 3_000,
    toHaveScreenshot: {
      maxDiffPixelRatio: 0.01,
      animations: "disabled",
    },
  },
  use: {
    baseURL: "http://localhost:3000",
    viewport: { width: 1280, height: 720 },
    actionTimeout: 3_000,
    navigationTimeout: 3_000,
  },
  webServer: {
    command: "npm run dev",
    port: 3000,
    reuseExistingServer: !process.env.CI,
  },
});
