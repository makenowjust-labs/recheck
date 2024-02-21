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

test("strategy: default (automaton)", () => {
  const checker = createCachedCheck({ location: cacheFile }, null, {
    checker: "automaton",
  });

  const result1 = checker("^a$", "");
  expect(JSON.parse(fs.readFileSync(cacheFile, "utf-8")).results).toEqual({
    "/^a$/": result1,
  });

  expect(checker("^a$", "")).toEqual(result1);
  const result2 = checker("^b$", "");
  expect(JSON.parse(fs.readFileSync(cacheFile, "utf-8")).results).toEqual({
    "/^a$/": result1,
    "/^b$/": result2,
  });
});

test("strategy: default (fuzz)", () => {
  const checker = createCachedCheck({ location: cacheFile }, null, {
    checker: "fuzz",
  });

  checker("^a$", "");
  expect(fs.existsSync(cacheFile)).toBe(false);
});

test("strategy: default (reuse)", () => {
  const checker1 = createCachedCheck({ location: cacheFile }, null, {
    checker: "automaton",
  });
  const result = checker1("^a$", "");
  expect(JSON.parse(fs.readFileSync(cacheFile, "utf-8")).results).toEqual({
    "/^a$/": result,
  });

  const checker2 = createCachedCheck({ location: cacheFile }, null, {
    checker: "automaton",
  });
  expect(checker2("^a$", "")).toEqual(result);
  expect(JSON.parse(fs.readFileSync(cacheFile, "utf-8")).results).toEqual({
    "/^a$/": result,
  });
});

test("strategy: aggressive", () => {
  const checker = createCachedCheck(
    { location: cacheFile, strategy: "aggressive" },
    null,
    { checker: "fuzz" },
  );

  const result1 = checker("^a$", "");
  expect(JSON.parse(fs.readFileSync(cacheFile, "utf-8")).results).toEqual({
    "/^a$/": result1,
  });

  expect(checker("^a$", "")).toEqual(result1);
  const result2 = checker("^b$", "");
  expect(JSON.parse(fs.readFileSync(cacheFile, "utf-8")).results).toEqual({
    "/^a$/": result1,
    "/^b$/": result2,
  });
});

test("strategy: conservative", () => {
  const checker = createCachedCheck(
    { location: cacheFile, strategy: "conservative" },
    null,
    { checker: "fuzz" },
  );

  checker("^a$", "");
  expect(fs.existsSync(cacheFile)).toBe(false);
});
