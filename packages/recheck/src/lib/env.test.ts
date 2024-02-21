import * as env from "./env";

beforeEach(() => {
  delete process.env["RECHECK_BACKEND"];
  delete process.env["RECHECK_BIN"];
  delete process.env["RECHECK_JAR"];
});

afterEach(() => {
  delete process.env["RECHECK_BACKEND"];
  delete process.env["RECHECK_SYNC_BACKEND"];
  delete process.env["RECHECK_BIN"];
  delete process.env["RECHECK_JAR"];
});

test("RECHECK_BACKEND", () => {
  expect(env.RECHECK_BACKEND()).toBe("auto");

  for (const value of ["auto", "java", "native", "pure"]) {
    process.env["RECHECK_BACKEND"] = value;
    expect(env.RECHECK_BACKEND()).toBe(value);
  }
});

test("RECHECK_SYNC_BACKEND", () => {
  expect(env.RECHECK_SYNC_BACKEND()).toBe("synckit");

  for (const value of ["synckit", "pure"]) {
    process.env["RECHECK_BACKEND"] = value;
    expect(env.RECHECK_BACKEND()).toBe(value);
  }
});

test("RECHECK_BIN", () => {
  expect(env.RECHECK_BIN()).toBeNull();

  process.env["RECHECK_BIN"] = "x";
  expect(env.RECHECK_BIN()).toBe("x");
});

test("RECHECK_JAR", () => {
  expect(env.RECHECK_JAR()).toBeNull();

  process.env["RECHECK_JAR"] = "x";
  expect(env.RECHECK_JAR()).toBe("x");
});
