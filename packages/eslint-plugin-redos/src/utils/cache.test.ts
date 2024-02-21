import * as os from "os";
import * as path from "path";
import { findCacheFile, __mock__ } from "./cache";

const cacheDir = os.tmpdir();
const defaultNodeModuleDir = __mock__.nodeModuleDir;

afterEach(() => {
  __mock__.nodeModuleDir = defaultNodeModuleDir;
});

test("with location", () => {
  const cacheLocation = findCacheFile(
    path.join(cacheDir, "recheck-cache-test.json"),
  );
  expect(cacheLocation).toBe(path.join(cacheDir, "recheck-cache-test.json"));
});

test("with location (directory)", () => {
  expect(() => findCacheFile(cacheDir)).toThrow(
    /Resolved cache\.location '.*' is a directory/,
  );
});

test("without location", () => {
  __mock__.nodeModuleDir = cacheDir;
  const cacheLocation = findCacheFile(undefined);
  expect(cacheLocation).toBe(
    path.join(cacheDir, ".cache/eslint-plugin-redos/recheck-cache.json"),
  );
});
