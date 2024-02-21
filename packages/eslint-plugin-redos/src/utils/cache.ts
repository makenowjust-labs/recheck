import * as fs from "fs";
import * as os from "os";
import * as path from "path";

/** A collection of variables to mock. */
type Mock = {
  nodeModuleDir: string;
};

/** Exposes this to mock `nodeModuleDir`. */
export const __mock__: Mock = {
  nodeModuleDir: path.join(
    require.resolve("eslint-plugin-redos/package.json"),
    "../..",
  ),
};

const findDefaultCacheFile = (): string => {
  let cacheDir: string | null = null;
  try {
    cacheDir = path.join(__mock__.nodeModuleDir, ".cache/eslint-plugin-redos");
    fs.mkdirSync(cacheDir, { recursive: true });
    /* c8 ignore next 3 */
  } catch {
    cacheDir = os.tmpdir();
  }

  const cacheFile = path.join(cacheDir, "recheck-cache.json");
  return cacheFile;
};

const findCacheFileFromOptions = (location: string): string => {
  const cacheFile = path.resolve(location);

  let stat;
  try {
    stat = fs.statSync(cacheFile);
  } catch {}

  if (stat) {
    if (stat.isDirectory()) {
      throw new Error(`Resolved cache.location '${cacheFile}' is a directory`);
    }
  }

  return cacheFile;
};

/** Returns a cache file path string. */
export const findCacheFile = (location: string | undefined): string => {
  if (!location) {
    return findDefaultCacheFile();
  }

  return findCacheFileFromOptions(location);
};
