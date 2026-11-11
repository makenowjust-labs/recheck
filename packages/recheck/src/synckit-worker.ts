import { runAsWorker } from "synckit";

/* @ts-expect-error */
import { check, repair } from "../lib/main.js";
import type { Diagnostics, RepairParameters, Rx } from "..";

runAsWorker(
  async (
    method: "check" | "repair",
    source: string,
    flags: string,
    params: RepairParameters,
  ): Promise<Diagnostics | Rx> => {
    return method === "repair"
      ? repair(source, flags, params)
      : check(source, flags, params);
  },
);
