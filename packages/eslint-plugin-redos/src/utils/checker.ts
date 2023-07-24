import * as fs from "fs";
import * as util from "util";

import * as ReDoS from "recheck";

import { findCacheFile } from "./cache";
import { recheckVersion } from "./version";

export type CacheStrategy = "aggressive" | "conservative";

export type CacheOptions = {
  location?: string;
  strategy?: CacheStrategy;
};

type CacheData = {
  settings: {
    version: string;
    timeout: number | null;
    strategy: CacheStrategy;
    params: ReDoS.Parameters;
  };
  results: Record<string, ReDoS.Diagnostics>;
};

export type Checker = (source: string, flags: string) => ReDoS.Diagnostics;

/** Returns a function to check the given regex is safe with caching. */
export const createCachedCheck = (
  cache: CacheOptions | boolean,
  timeout: number | null,
  params: ReDoS.Parameters,
): Checker => {
  const {
    location: cacheLocation = undefined,
    strategy: cacheStrategy = "conservative",
  } = typeof cache === "boolean" ? {} : cache;
  const cacheFile = cache ? findCacheFile(cacheLocation) : null;

  const settings = {
    version: recheckVersion(),
    timeout,
    strategy: cacheStrategy,
    params,
  };

  let cacheData: CacheData;
  try {
    if (cacheFile) {
      cacheData = fs.existsSync(cacheFile)
        ? JSON.parse(fs.readFileSync(cacheFile, "utf-8"))
        : {};

      // Purge the cache if it is invalidated.
      if (!util.isDeepStrictEqual(cacheData.settings, settings)) {
        try {
          fs.rmSync(cacheFile);
        } catch {}
        cacheData = {
          settings,
          results: {},
        };
      }
    }
  } catch (error) {
    throw new Error(`Invalid cache: ${error}`);
  }

  const cachedCheck = (source: string, flags: string) => {
    const key = `/${source}/${flags}`;
    if (cacheData && cacheData.results[key]) {
      // A cache is found, we use it.
      return cacheData.results[key];
    }

    // TODO: We want to use `ReDoS.check` (asynchronous version) instead.
    // However, it is impossible due to the ESLint limitation.
    const result = ReDoS.checkSync(source, flags, { timeout, ...params });

    let shouldCache = false;
    switch (cacheStrategy) {
      case "aggressive":
        shouldCache = true;
        break;
      case "conservative":
        shouldCache = result.checker === "automaton";
        break;
    }

    if (!shouldCache) {
      return result;
    }

    if (cacheFile) {
      cacheData.results[key] = result;
      // TODO: Delay the writing out the cache.
      fs.writeFileSync(cacheFile, JSON.stringify(cacheData));
    }

    return result;
  };

  return cachedCheck;
};
