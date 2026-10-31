// @ts-expect-error
import * as bundle from "../../../../target/recheck-js-opt/recheck.js";

import type { Diagnostics, Parameters } from "../..";

/** The well-typed shortcut to Scala.js (pure JavaScript) implementation. */
export function check(
  source: string,
  flags: string,
  params: Parameters = {},
): Diagnostics {
  return bundle.check(source, flags, params);
}

/** The well-typed shortcut to Scala.js (pure JavaScript) implementation of `checkAll`. */
export function checkAll(
  source: string,
  flags: string,
  params: Parameters = {},
): Diagnostics[] {
  return bundle.checkAll(source, flags, params);
}
