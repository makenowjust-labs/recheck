// This file provides the fallback implementation of `check` function.
// It uses Scala.js build, so we can run `recheck` on browser.
// The checker runs on the same process, so we cannot cancel the execution interactively.

// @ts-expect-error
import * as bundle from "../../../modules/recheck-js/target/scala-2.13/recheck-js-opt/recheck";

import type { Config, Diagnostics } from "..";

export async function check(
  source: string,
  flags: string,
  config: Config = {}
): Promise<Diagnostics> {
  return await new Promise((resolve) =>
    resolve(bundle.check(source, flags, config))
  );
}
