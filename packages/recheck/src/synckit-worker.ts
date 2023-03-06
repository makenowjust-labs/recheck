import { runAsWorker } from "synckit";

/* @ts-expect-error */
import { check } from "./lib/main.js";
import type { Diagnostics, Parameters } from "..";

runAsWorker(
  async (
    source: string,
    flags: string,
    params: Parameters
  ): Promise<Diagnostics> => {
    return check(source, flags, params);
  }
);
