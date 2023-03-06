import * as fs from "fs";
import * as os from "os";
import * as path from "path";
import { createCachedCheck } from "./checker";

const cacheDir = os.tmpdir();
const cacheFile = path.join(cacheDir, "recheck-cache-test.json");

afterEach(() => {
  try {
    fs.rmSync(cacheFile);
  } catch {}
});

test("strategy: default", () => {
  const checker = createCachedCheck({ location: cacheFile }, null, {
    checker: "automaton",
  });
  const result = checker("^a$", "");
  expect(JSON.parse(fs.readFileSync(cacheFile, "utf-8")).results).toEqual({
    "/^a$/": result,
  });
});

test("strategy: aggressive", () => {
  const checker = createCachedCheck(
    { location: cacheFile, strategy: "aggressive" },
    null,
    { checker: "fuzz" }
  );
  const result = checker("^a$", "");
  expect(JSON.parse(fs.readFileSync(cacheFile, "utf-8")).results).toEqual({
    "/^a$/": result,
  });
});

test("strategy: conservative", () => {
  const checker = createCachedCheck(
    { location: cacheFile, strategy: "conservative" },
    null,
    { checker: "fuzz" }
  );
  const result = checker("^a$", "");
  expect(fs.existsSync(cacheFile)).toBe(false);
});
