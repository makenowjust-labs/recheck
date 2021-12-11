// This file provides the `check` function for Node.js.

import { debuglog } from "util";

import { check as fallbackCheck } from "./fallback";
import { check as agentCheck, ensureAgent } from "./agent";

import type { Diagnostics, HasAbortSignal, Parameters } from "..";

export { checkSync } from "./fallback";

const debug = debuglog("recheck");

export async function check(
  source: string,
  flags: string,
  params: Parameters & HasAbortSignal = {}
): Promise<Diagnostics> {
  const RECHECK_MODE = process.env["RECHECK_MODE"] ?? "";
  debug("`recheck` mode: %s", RECHECK_MODE);
  switch (RECHECK_MODE) {
    case "agent":
      return await agentCheck(source, flags, params);
    case "fallback":
      return await fallbackCheck(source, flags, params);
  }

  const signal = params.signal ?? null;
  if (signal) {
    delete params.signal;
  }

  const agent = ensureAgent();
  if (agent === null) {
    return await fallbackCheck(source, flags, params);
  }

  const { id, promise } = agent.request("check", { source, flags, params });
  signal?.addEventListener("abort", () => {
    agent.notify("cancel", { id });
  });

  return await promise;
}
