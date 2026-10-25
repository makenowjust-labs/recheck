import path from "node:path";

import { expect, test as base } from "@playwright/test";

import { parseParams } from "../src/components/Demo/params";
import { encode } from "../src/components/Demo/permalink";

import type { Page } from "@playwright/test";
import type { Snapshot } from "../src/components/Demo/permalink";

const CDN_MODULE = "https://cdn.jsdelivr.net/npm/recheck@*/lib/browser.mjs";
const LOCAL_MODULE = path.join(
  path.dirname(require.resolve("recheck/package.json")),
  "lib/browser.mjs",
);

const test = base.extend<{ cdn: string[]; errors: string[] }>({
  // Releases are served from the local build so that the tests do not depend on the CDN.
  cdn: [
    async ({ page }, use) => {
      const requests: string[] = [];
      await page.route(CDN_MODULE, (route) => {
        requests.push(route.request().url());
        return route.fulfill({
          path: LOCAL_MODULE,
          contentType: "text/javascript",
          headers: { "access-control-allow-origin": "*" },
        });
      });
      await use(requests);
    },
    { auto: true },
  ],
  errors: [
    async ({ page }, use) => {
      const errors: string[] = [];
      page.on("pageerror", (error) => errors.push(error.message));
      page.on("console", (message) => {
        // A blocked request is reported here too, and the tests assert on how the page handles it.
        if (
          message.type() === "error" &&
          !message.text().startsWith("Failed to load resource")
        ) {
          errors.push(message.text());
        }
      });
      await use(errors);
      expect(errors).toEqual([]);
    },
    { auto: true },
  ],
});

const open = async (page: Page, hash = ""): Promise<void> => {
  await page.goto(`playground/${hash}`);
  await page.waitForLoadState("networkidle");
};

const input = (page: Page) => page.locator('textarea[name="input"]');
const check = (page: Page) =>
  page.getByRole("button", { name: "Check" }).click();
const resultTitles = (page: Page) =>
  page.locator(".card__header h3", { hasText: "Result" });
const footers = (page: Page) => page.locator(".card__footer");

test("runs the input with the chosen build and records a permalink", async ({
  page,
  cdn,
}) => {
  await open(page);
  const version = page.getByLabel("recheck version");
  const labels = await version.locator("option").allTextContents();
  const release = labels[0]!;
  const bundled = labels.at(-1)!;
  expect(release).toMatch(/^v\d/);

  await input(page).fill("/^(a|a)*$/");
  await check(page);
  await expect(page.locator(".badge", { hasText: "vulnerable" })).toBeVisible();
  await expect(footers(page)).toContainText(`recheck ${release}`);
  expect(cdn).toHaveLength(1);
  expect(new URL(page.url()).hash).toMatch(/^#r=/);

  await version.selectOption(bundled);
  await check(page);
  await expect(footers(page)).toContainText(`recheck ${bundled}`);
  expect(cdn).toHaveLength(1);
});

test("restores a permalink and compares a new run against it", async ({
  page,
}) => {
  const recorded: Snapshot = {
    v: 1,
    build: { type: "release", version: "0.0.1" },
    input: "/a/",
    params: parseParams({ checker: "automaton" }),
    result: { status: "safe", source: "a", flags: "", checker: "automaton" },
    time: 1,
    date: "2026-10-25T00:00:00.000Z",
  };
  await open(page, `#${encode(recorded)}`);
  await expect(input(page)).toHaveValue("/a/");
  await expect(page.locator('select[name="checker"]')).toHaveValue("automaton");
  await expect(page.locator(".card__header .badge")).toHaveText("recorded");
  await expect(footers(page)).toContainText("recheck v0.0.1");
  await expect(page.locator(".alert--warning")).toContainText(
    "recorded with recheck v0.0.1",
  );

  await check(page);
  await expect(page.locator(".alert--success")).toHaveText(
    "The result matches the recorded one.",
  );
  await expect(resultTitles(page)).toHaveCount(2);

  await input(page).fill("/b/");
  await check(page);
  await expect(resultTitles(page)).toHaveCount(1);
  await expect(page.locator(".card__header .badge")).toHaveCount(0);
});

test("reports a release that cannot be loaded", async ({ page }) => {
  await page.route(CDN_MODULE, (route) => route.abort());
  await open(page);
  await check(page);
  await expect(page.locator(".alert--danger")).toContainText(
    /^Failed to load recheck v/,
  );
  await expect(resultTitles(page)).toHaveCount(0);
});
