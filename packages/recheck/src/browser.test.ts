import * as browser from "./browser";

test("check", async () => {
  const diagnostics = await browser.check("^(a|a)+$", "");
  expect(diagnostics.status).toBe("vulnerable");
});

test("checkSync", () => {
  const diagnostics = browser.checkSync("^(a|a)+$", "");
  expect(diagnostics.status).toBe("vulnerable");
});

test("repair", async () => {
  // The worker is the fake one of the tests, which returns its method and `keep`.
  const rx = await browser.repair("^(a+)+$", "", { keep: "match" });
  expect(rx).toEqual({ status: "safe", method: "repair", keep: "match" });
});

test("repairSync", () => {
  const rx = browser.repairSync("^(a+)+$", "");
  expect(rx).toMatchObject({ status: "repaired", repaired: "^(a+)$" });
});
