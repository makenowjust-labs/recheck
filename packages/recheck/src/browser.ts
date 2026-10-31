import * as pure from "./lib/pure";
import { WorkerPool } from "./lib/worker-pool";

import type { Diagnostics, HasAbortSignal, Parameters } from "..";

let pool: WorkerPool | null = null;

const getPool = (): WorkerPool => {
  if (pool === null) {
    pool = new WorkerPool(1);
  }
  return pool;
};

export async function check(
  source: string,
  flags: string,
  params: Parameters & HasAbortSignal = {},
): Promise<Diagnostics> {
  return getPool().check(source, flags, params);
}

export async function checkAll(
  source: string,
  flags: string,
  params: Parameters & HasAbortSignal = {},
): Promise<Diagnostics[]> {
  return getPool().checkAll(source, flags, params);
}

export const checkSync = pure.check;

export const checkAllSync = pure.checkAll;
