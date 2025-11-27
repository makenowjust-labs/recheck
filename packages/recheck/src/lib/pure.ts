// @ts-expect-error
import * as bundle from "../../../../modules/recheck-js/target/scala-3.7.4/recheck-js-opt/recheck.js";

import type { Diagnostics, Parameters } from "../..";

/** The well-typed shortcut to Scala.js (pure JavaScript) implementation. */
export function check(
  source: string,
  flags: string,
  params: Parameters = {},
): Diagnostics {
  return bundle.check(source, flags, params);
}
