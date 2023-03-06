import { runAsWorker } from "synckit";

import { check } from "./main";
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
