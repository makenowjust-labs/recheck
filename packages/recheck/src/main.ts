import { createSyncFn } from "synckit";

import { check as checkAgent } from "./lib/agent";
import * as env from "./lib/env";
import * as java from "./lib/java";
import * as native from "./lib/native";
import * as pure from "./lib/pure";
import { WorkerPool } from "./lib/worker-pool";

import type { Diagnostics, HasAbortSignal, Parameters } from "..";
import type { Agent } from "./lib/agent";

/** A collection of variables to mock. */
type Mock = {
  agent: Agent | null | undefined;
  pool: WorkerPool | null;
};

/** Exposes this to mock `agent` and `pool`. */
export const __mock__: Mock = {
  agent: undefined,
  pool: null,
};

/** `check` implementation. */
export async function check(
  source: string,
  flags: string,
  params: Parameters & HasAbortSignal = {}
): Promise<Diagnostics> {
  const backend = env.RECHECK_BACKEND();
  switch (backend) {
    case "auto":
      if (__mock__.agent === undefined) {
        try {
          /* c8 ignore next */
          __mock__.agent = (await native.ensure()) ?? undefined;
        } catch {
          __mock__.agent = undefined;
        }
      }
      if (__mock__.agent === undefined) {
        try {
          /* c8 ignore next */
          __mock__.agent = (await java.ensure()) ?? undefined;
        } catch {
          __mock__.agent = undefined;
        }
      }
      if (__mock__.agent === undefined) {
        __mock__.agent = null;
      }
      break;
    case "java":
      if (__mock__.agent === undefined) {
        __mock__.agent = await java.ensure();
      }
      break;
    case "native":
      if (__mock__.agent === undefined) {
        __mock__.agent = await native.ensure();
      }
      break;
    case "worker":
      if (__mock__.pool === null) {
        __mock__.pool = new WorkerPool(1);
      }
      return __mock__.pool.check(source, flags, params);
    case "pure":
      return pure.check(source, flags, params);
    default:
      throw new Error(`invalid backend: ${backend}`);
  }

  const agent = __mock__.agent;
  if (agent === null) {
    if (backend !== "auto") {
      throw new Error("there is no available implementation");
    }

    if (__mock__.pool === null) {
      __mock__.pool = new WorkerPool(1);
    }
    return __mock__.pool.check(source, flags, params);
  }

  return checkAgent(agent, source, flags, params);
}

let syncFnCache: typeof pure.check | null = null;

export const checkSync = (
  source: string,
  flags: string,
  params: Parameters = {}
): Diagnostics => {
  let syncFn: typeof pure.check | null = null;
  const backend = env.RECHECK_SYNC_BACKEND();
  switch (backend) {
    case "synckit":
      if (syncFnCache === null) {
        syncFnCache = createSyncFn(require.resolve("./synckit-worker"));
      }
      syncFn = syncFnCache;
      break;
    case "pure":
      syncFn = pure.check;
      break;
  }
  if (syncFn === null) {
    throw new Error(`invalid sync backend: ${backend}`);
  }

  return syncFn(source, flags, params);
};
