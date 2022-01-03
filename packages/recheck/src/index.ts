import { check as checkAgent } from "./lib/agent";
import * as env from "./lib/env";
import * as java from "./lib/java";
import * as native from "./lib/native";
import * as pure from "./lib/pure";

import type { Diagnostics, HasAbortSignal, Parameters } from "..";
import type { Agent } from "./lib/agent";

/** Exposes this to mock `agent`. */
export const __mock__: { agent: Agent | null | undefined } = {
  agent: undefined,
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
          /* c8 ignore next 1 */
          __mock__.agent = (await java.ensure()) ?? undefined;
        } catch {
          __mock__.agent = undefined;
        }
      }
      if (__mock__.agent === undefined) {
        try {
          /* c8 ignore next 1 */
          __mock__.agent = (await native.ensure()) ?? undefined;
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
    return pure.check(source, flags, params);
  }

  return checkAgent(agent!, source, flags, params);
}

export const checkSync = pure.check;
