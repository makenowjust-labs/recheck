import { createSyncFn } from "synckit";

import { check as checkAgent } from "./lib/agent";
import * as env from "./lib/env";
import { nodeRequire } from "./require";
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

type Backend =
  | { type: "agent"; agent: Agent }
  | { type: "pool"; pool: WorkerPool }
  | { type: "pure" };

const validateArguments = (source: unknown, flags: unknown): void => {
  /* c8 ignore start */
  if (typeof source !== "string") {
    throw new TypeError(
      `Expected 'source' to be a string, but got ${typeof source}`,
    );
  }
  if (typeof flags !== "string") {
    throw new TypeError(
      `Expected 'flags' to be a string, but got ${typeof flags}`,
    );
  }
  /* c8 ignore stop */
};

const ensurePoolBackend = (): Backend => {
  if (__mock__.pool === null) {
    __mock__.pool = new WorkerPool(1);
  }
  return { type: "pool", pool: __mock__.pool };
};

/** Returns the backend of `RECHECK_BACKEND`, starting the agent on the first call. */
async function selectBackend(): Promise<Backend> {
  const name = env.RECHECK_BACKEND();
  switch (name) {
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
      return ensurePoolBackend();
    case "pure":
      return { type: "pure" };
    default:
      throw new Error(`invalid backend: ${name}`);
  }

  const agent = __mock__.agent;
  if (agent === null) {
    if (name !== "auto") {
      throw new Error("there is no available implementation");
    }
    return ensurePoolBackend();
  }

  return { type: "agent", agent };
}

/** `check` implementation. */
export async function check(
  source: string,
  flags: string,
  params: Parameters & HasAbortSignal = {},
): Promise<Diagnostics> {
  validateArguments(source, flags);
  const b = await selectBackend();
  switch (b.type) {
    case "agent":
      return checkAgent(b.agent, source, flags, params);
    case "pool":
      return b.pool.check(source, flags, params);
    case "pure":
      return pure.check(source, flags, params);
  }
}

type SyncFn = (
  source: string,
  flags: string,
  params: Parameters,
) => Diagnostics;

/** The function calling the synckit worker, which is created on the first use. */
let synckitFn: SyncFn | null = null;

/** Returns the function of `RECHECK_SYNC_BACKEND`. */
const selectSyncFn = (): SyncFn => {
  const name = env.RECHECK_SYNC_BACKEND();
  switch (name) {
    case "synckit":
      if (synckitFn === null) {
        synckitFn = createSyncFn<SyncFn>(
          nodeRequire.resolve("./synckit-worker"),
        );
      }
      return synckitFn;
    case "pure":
      return pure.check;
  }
  throw new Error(`invalid sync backend: ${name}`);
};

export const checkSync = (
  source: string,
  flags: string,
  params: Parameters = {},
): Diagnostics => {
  validateArguments(source, flags);
  return selectSyncFn()(source, flags, params);
};
