import * as pure from "./lib/pure";
import { WorkerPool } from "./lib/worker-pool";

import type { Diagnostics, HasAbortSignal, Parameters } from "..";

let pool: WorkerPool | null = null;

export async function check(
  source: string,
  flags: string,
  params: Parameters & HasAbortSignal = {}
): Promise<Diagnostics> {
  if (pool === null) {
    pool = new WorkerPool(1);
  }

  return pool.check(source, flags, params);
}

export const checkSync = pure.check;
