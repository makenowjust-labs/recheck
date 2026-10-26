import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";

import { afterEach, beforeEach, expect, jest, test } from "@jest/globals";

import * as exe from "./exe";

const isSupported =
  typeof exe.osNames[process.platform] !== "undefined" &&
  typeof exe.cpuNames[process.arch] !== "undefined";

let dir: string;

beforeEach(() => {
  dir = fs.mkdtempSync(path.join(os.tmpdir(), "recheck-exe-"));
  delete process.env["RECHECK_BIN"];
  delete process.env["RECHECK_JAR"];
});

afterEach(() => {
  jest.restoreAllMocks();
  fs.rmSync(dir, { recursive: true });
  delete process.env["RECHECK_BIN"];
  delete process.env["RECHECK_JAR"];
});

// Lets `require.resolve` find `pkg` in the temporary directory, with `file` next to its `package.json` or missing.
const install = (pkg: string, file: string | null): string => {
  const root = path.join(dir, pkg);
  fs.mkdirSync(root, { recursive: true });
  jest
    .spyOn(exe.__mock__require, "resolve")
    .mockReturnValueOnce(path.join(root, "package.json"));
  if (file === null) return root;
  fs.writeFileSync(path.join(root, file), "");
  return path.join(root, file);
};

const failResolve = (code: string | null): void => {
  jest.spyOn(exe.__mock__require, "resolve").mockImplementationOnce(() => {
    const err: any = new Error("resolve failed");
    if (code !== null) err.code = code;
    throw err;
  });
};

test("jar", () => {
  const jar = install("recheck-jar", "recheck.jar");
  expect(exe.jar()).toBe(jar);

  process.env["RECHECK_JAR"] = "x";
  expect(exe.jar()).toBe("x");
});

test("jar: not found", () => {
  install("recheck-jar", null);
  expect(exe.jar()).toBeNull();
});

test("jar: invalid resolve (1)", () => {
  failResolve("MODULE_NOT_FOUND");
  expect(exe.jar()).toBeNull();
});

test("jar: invalid resolve (2)", () => {
  failResolve(null);
  expect(() => exe.jar()).toThrow();
});

test("bin", () => {
  if (isSupported) {
    const pkg = `recheck-${exe.osNames[process.platform]}-${exe.cpuNames[process.arch]}`;
    const bin = install(
      pkg,
      process.platform === "win32" ? "recheck.exe" : "recheck",
    );
    expect(exe.bin()).toBe(bin);
  }

  process.env["RECHECK_BIN"] = "x";
  expect(exe.bin()).toBe("x");
});

test("bin: not found", () => {
  if (!isSupported) {
    return;
  }

  install(
    `recheck-${exe.osNames[process.platform]}-${exe.cpuNames[process.arch]}`,
    null,
  );
  expect(exe.bin()).toBeNull();
});

test("bin: invalid resolve (1)", () => {
  if (!isSupported) {
    return;
  }

  failResolve("MODULE_NOT_FOUND");
  expect(exe.bin()).toBeNull();
});

test("bin: invalid resolve (2)", () => {
  if (!isSupported) {
    return;
  }

  failResolve(null);
  expect(() => exe.bin()).toThrow();
});
