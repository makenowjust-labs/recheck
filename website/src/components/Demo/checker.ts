import * as bundled from "recheck/lib/browser";

import { moduleUrl } from "./builds";

import type { Build } from "./permalink";

export type Checker = Pick<typeof bundled, "check">;

const loading = new Map<string, Promise<Checker>>();

// Babel rewrites `import()` to `require()` for the server bundle, which makes webpack warn about a dynamic request.
const importUrl = new Function("url", "return import(url)") as (
  url: string,
) => Promise<Checker>;

/** Resolves the `recheck` module for the build, fetching releases from the CDN once. */
export const loadChecker = (build: Build): Promise<Checker> => {
  if (build.type !== "release") return Promise.resolve(bundled);
  let promise = loading.get(build.version);
  if (promise === undefined) {
    promise = importUrl(moduleUrl(build.version));
    loading.set(build.version, promise);
    promise.catch(() => loading.delete(build.version));
  }
  return promise;
};
