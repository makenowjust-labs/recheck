// Copies the `Parameters` type into this package, because the published types must not import the private `recheck-parameters`.

import { copyFileSync } from "node:fs";

copyFileSync(
  new URL(import.meta.resolve("recheck-parameters/parameters.d.ts")),
  new URL("../parameters.d.ts", import.meta.url),
);
