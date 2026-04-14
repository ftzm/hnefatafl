import { defineConfig } from "vite";
import solidPlugin from "vite-plugin-solid";

export default defineConfig({
  plugins: [solidPlugin()],
  server: {
    port: 3000,
    host: true,
    proxy: {
      "/hotseat": "http://localhost:8080",
      "/ai": {
        target: "http://localhost:8080",
        ws: true,
      },
      "/online": {
        target: "http://localhost:8080",
        ws: true,
      },
      "/health": "http://localhost:8080",
      "/version": "http://localhost:8080",
    },
  },
  build: {
    target: "esnext",
  },
});
