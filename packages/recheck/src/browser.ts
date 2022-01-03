import * as pure from "./lib/pure";

import type { Diagnostics, HasAbortSignal, Parameters } from "..";

export async function check(
  source: string,
  flags: string,
  params: Parameters & HasAbortSignal = {}
): Promise<Diagnostics> {
  return pure.check(source, flags, params);
}

export const checkSync = pure.check;
