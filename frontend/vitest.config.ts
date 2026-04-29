import solid from "vite-plugin-solid";
import { defineConfig } from "vitest/config";

export default defineConfig({
  plugins: [solid()],
  resolve: {
    conditions: ["development", "browser"],
  },
  server: {
    // In the Nix build sandbox, node_modules is a symlink into /nix/store,
    // which is outside the project root. Vite's default fs.strict blocks
    // serving files from there. Disable strict mode for tests so vitest can
    // load test helpers that resolve through the symlinked node_modules.
    fs: { strict: false },
  },
  test: {
    environment: "jsdom",
    setupFiles: ["./src/test-setup.ts"],
    exclude: ["e2e/**", "node_modules/**"],
  },
});
