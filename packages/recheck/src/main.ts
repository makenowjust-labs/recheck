import { createSyncFn } from "synckit";

import * as agent from "./lib/agent";
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
  syncFn: SyncFn | null;
};

/** Exposes this to mock `agent`, `pool` and `syncFn`. */
export const __mock__: Mock = {
  agent: undefined,
  pool: null,
  syncFn: null,
};

/** Backend is what runs a check. */
type Backend =
  | { type: "agent"; agent: Agent }
  | { type: "pool"; pool: WorkerPool }
  | { type: "pure" };

/** Throws a `TypeError` if `source` or `flags` is not a string. */
const validate = (source: unknown, flags: unknown): void => {
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

/** Returns the worker pool, creating it on the first call. */
const pool = (): Backend => {
  if (__mock__.pool === null) {
    __mock__.pool = new WorkerPool(1);
  }
  return { type: "pool", pool: __mock__.pool };
};

/** Returns the backend of `RECHECK_BACKEND`, starting the agent on the first call. */
async function backend(): Promise<Backend> {
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
      return pool();
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
    return pool();
  }

  return { type: "agent", agent };
}

/** `check` implementation. */
export async function check(
  source: string,
  flags: string,
  params: Parameters & HasAbortSignal = {},
): Promise<Diagnostics> {
  validate(source, flags);
  const b = await backend();
  switch (b.type) {
    case "agent":
      return agent.check(b.agent, source, flags, params);
    case "pool":
      return b.pool.check(source, flags, params);
    case "pure":
      return pure.check(source, flags, params);
  }
}

/** `checkAll` implementation. */
export async function checkAll(
  source: string,
  flags: string,
  params: Parameters & HasAbortSignal = {},
): Promise<Diagnostics[]> {
  validate(source, flags);
  const b = await backend();
  switch (b.type) {
    case "agent":
      return agent.checkAll(b.agent, source, flags, params);
    case "pool":
      return b.pool.checkAll(source, flags, params);
    case "pure":
      return pure.checkAll(source, flags, params);
  }
}

/** A synchronous function running a checking method of `RECHECK_BACKEND` in a worker. */
type SyncFn = (
  method: "check" | "checkAll",
  source: string,
  flags: string,
  params: Parameters,
) => any;

/** Returns the synchronous function of `RECHECK_SYNC_BACKEND`. */
const syncFn = (): SyncFn => {
  const backend = env.RECHECK_SYNC_BACKEND();
  switch (backend) {
    case "synckit":
      if (__mock__.syncFn === null) {
        __mock__.syncFn = createSyncFn<SyncFn>(
          nodeRequire.resolve("./synckit-worker"),
        );
      }
      return __mock__.syncFn;
    case "pure":
      return (method, source, flags, params) =>
        method === "checkAll"
          ? pure.checkAll(source, flags, params)
          : pure.check(source, flags, params);
  }
  throw new Error(`invalid sync backend: ${backend}`);
};

export const checkSync = (
  source: string,
  flags: string,
  params: Parameters = {},
): Diagnostics => {
  validate(source, flags);
  return syncFn()("check", source, flags, params);
};

export const checkAllSync = (
  source: string,
  flags: string,
  params: Parameters = {},
): Diagnostics[] => {
  validate(source, flags);
  return syncFn()("checkAll", source, flags, params);
};
