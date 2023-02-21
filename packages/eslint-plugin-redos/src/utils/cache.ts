import * as fs from "fs";
import * as os from "os";
import * as path from "path";

import findCacheDir from "find-cache-dir";

const findDefaultCacheFile = (): string => {
  const cacheDir =
    findCacheDir({ name: "eslint-plugin-redos", create: true }) || os.tmpdir();
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
