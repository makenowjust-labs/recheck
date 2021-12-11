// This file provides the fallback implementation of `check` function.
// It uses Scala.js build, so we can run `recheck` on browser.
// The checker runs on the same process, so we cannot cancel the execution interactively.

// @ts-expect-error
import * as bundle from "../../../modules/recheck-js/target/scala-2.13/recheck-js-opt/recheck";

import type { Diagnostics, HasAbortSignal, Parameters } from "..";

export async function check(
  source: string,
  flags: string,
  params: Parameters & HasAbortSignal = {}
): Promise<Diagnostics> {
  return await new Promise((resolve) => {
    resolve(bundle.check(source, flags, params));
  });
}

export const checkSync = bundle.check;
