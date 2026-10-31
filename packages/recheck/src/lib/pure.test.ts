import * as pure from "./pure";

test("check", () => {
  expect(pure.check("^(a|a)+$", "").status).toBe("vulnerable");
});

test("checkAll", () => {
  expect(pure.checkAll("^(a|a)+$", "").map((d) => d.status)).toEqual([
    "vulnerable",
  ]);
});
