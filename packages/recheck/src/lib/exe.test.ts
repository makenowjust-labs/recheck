import * as exe from "./exe";

const isSupported =
  typeof exe.osNames[process.platform] !== "undefined" &&
  typeof exe.cpuNames[process.arch] !== "undefined";

beforeEach(() => {
  delete process.env["RECHECK_BIN"];
  delete process.env["RECHECK_JAR"];
});

afterEach(() => {
  delete process.env["RECHECK_BIN"];
  delete process.env["RECHECK_JAR"];
});

test("jar", () => {
  expect(exe.jar()).toMatch(/recheck-jar\/recheck\.jar$/);

  process.env["RECHECK_JAR"] = "x";
  expect(exe.jar()).toBe("x");
});

test("jar: invalid resolve (1)", () => {
  const resolve = jest.spyOn(exe.__mock__require, "resolve");
  resolve.mockImplementationOnce(() => {
    const err: any = new Error("module not found");
    err.code = "MODULE_NOT_FOUND";
    throw err;
  });

  expect(exe.jar()).toBeNull();
});

test("jar: invalid resolve (2)", () => {
  const resolve = jest.spyOn(exe.__mock__require, "resolve");
  resolve.mockImplementationOnce(() => {
    const err: any = new Error("unknown error");
    throw err;
  });

  expect(() => exe.jar()).toThrow();
});

test("bin", () => {
  if (isSupported) {
    expect(exe.bin()).toMatch(/recheck-\w+-\w+\/recheck(?:\.exe)?$/);
  }

  process.env["RECHECK_BIN"] = "x";
  expect(exe.bin()).toBe("x");
});

test("bin: invalid resolve (1)", () => {
  if (!isSupported) {
    return;
  }

  const resolve = jest.spyOn(exe.__mock__require, "resolve");
  resolve.mockImplementationOnce(() => {
    const err: any = new Error("module not found");
    err.code = "MODULE_NOT_FOUND";
    throw err;
  });

  expect(exe.bin()).toBeNull();
});

test("bin: invalid resolve (2)", () => {
  if (!isSupported) {
    return;
  }

  const resolve = jest.spyOn(exe.__mock__require, "resolve");
  resolve.mockImplementationOnce(() => {
    const err: any = new Error("unknown error");
    throw err;
  });

  expect(() => exe.bin()).toThrow();
});
