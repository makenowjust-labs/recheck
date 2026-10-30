import { defineConfig } from "@playwright/test";

const port = 3005;

// The tests run against the production build, so run `pnpm run doc` before `pnpm run test:e2e`.
export default defineConfig({
  testDir: "./e2e",
  forbidOnly: !!process.env["CI"],
  use: {
    baseURL: `http://localhost:${port}/recheck/`,
    channel: "chrome",
  },
  webServer: {
    command: `pnpm exec docusaurus serve --port ${port} --no-open`,
    url: `http://localhost:${port}/recheck/`,
    reuseExistingServer: !process.env["CI"],
  },
});
