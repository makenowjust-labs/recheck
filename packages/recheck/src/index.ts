// This file provides the `check` function for Node.js.

import { debuglog } from "util";

import { check as fallbackCheck } from "./fallback";
import { check as agentCheck, ensureAgent } from "./agent";

import type { Config, Diagnostics } from "..";

export { checkSync } from "./fallback";

const debug = debuglog("recheck");

export async function check(
  source: string,
  flags: string,
  config: Config = {}
): Promise<Diagnostics> {
  const RECHECK_MODE = process.env["RECHECK_MODE"] ?? "";
  debug("`recheck` mode: %s", RECHECK_MODE);
  switch (RECHECK_MODE) {
    case "agent":
      return await agentCheck(source, flags, config);
    case "fallback":
      return await fallbackCheck(source, flags, config);
  }

  const signal = config.signal ?? null;
  if (signal) {
    delete config.signal;
  }

  const agent = ensureAgent();
  if (agent === null) {
    return await fallbackCheck(source, flags, config);
  }

  const { id, promise } = agent.request("check", { source, flags, config });
  signal?.addEventListener("abort", () => {
    agent.notify("cancel", { id });
  });

  return await promise;
}
