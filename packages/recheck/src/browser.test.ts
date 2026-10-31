import * as browser from "./browser";

test("check", async () => {
  const diagnostics = await browser.check("^(a|a)+$", "");
  expect(diagnostics.status).toBe("vulnerable");
});

test("checkSync", () => {
  const diagnostics = browser.checkSync("^(a|a)+$", "");
  expect(diagnostics.status).toBe("vulnerable");
});

test("checkAll", async () => {
  const diagnostics = await browser.checkAll("^(a|a)+$", "");
  expect(diagnostics.map((d) => d.status)).toEqual(["vulnerable"]);
});

test("checkAllSync", () => {
  const diagnostics = browser.checkAllSync("^(a|a)+$", "");
  expect(diagnostics.map((d) => d.status)).toEqual(["vulnerable"]);
});
