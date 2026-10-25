import * as bundled from "recheck/lib/browser";

import { moduleUrl } from "./builds";
import { importModule } from "./importModule";

import type { Build } from "./builds";

export type Checker = Pick<typeof bundled, "check">;

const loading = new Map<string, Promise<Checker>>();

/** Resolves the `recheck` module for the build, fetching releases from the CDN once. */
export const loadChecker = (build: Build): Promise<Checker> => {
  if (build.type !== "release") return Promise.resolve(bundled);
  let promise = loading.get(build.version);
  if (promise === undefined) {
    promise = importModule(moduleUrl(build.version)) as Promise<Checker>;
    loading.set(build.version, promise);
    promise.catch(() => loading.delete(build.version));
  }
  return promise;
};
