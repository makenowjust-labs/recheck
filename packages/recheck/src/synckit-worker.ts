import { runAsWorker } from "synckit";

/* @ts-expect-error */
import { check, checkAll } from "../lib/main.js";
import type { Diagnostics, Parameters } from "..";

runAsWorker(
  async (
    method: "check" | "checkAll",
    source: string,
    flags: string,
    params: Parameters,
  ): Promise<Diagnostics | Diagnostics[]> => {
    return method === "checkAll"
      ? checkAll(source, flags, params)
      : check(source, flags, params);
  },
);
