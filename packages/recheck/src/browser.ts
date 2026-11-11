import * as pure from "./lib/pure";
import { WorkerPool } from "./lib/worker-pool";

import type {
  Diagnostics,
  HasAbortSignal,
  Parameters,
  RepairParameters,
  Rx,
} from "..";

let pool: WorkerPool | null = null;

/** Returns the worker pool, which is created on the first use. */
const workerPool = (): WorkerPool => (pool ??= new WorkerPool(1));

export async function check(
  source: string,
  flags: string,
  params: Parameters & HasAbortSignal = {},
): Promise<Diagnostics> {
  return workerPool().check(source, flags, params);
}

export const checkSync = pure.check;

export async function repair(
  source: string,
  flags: string,
  params: RepairParameters & HasAbortSignal = {},
): Promise<Rx> {
  return workerPool().repair(source, flags, params);
}

export const repairSync = pure.repair;
