import * as pure from "./pure";

test("check", () => {
  expect(pure.check("^(a|a)+$", "").status).toBe("vulnerable");
});
