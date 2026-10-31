// Copies the `Parameters` and `Dialect` types into this package, because the published types must not import the private `recheck-parameters`.

import { copyFileSync } from "node:fs";

for (const name of ["parameters.d.ts", "dialect.d.ts"]) {
  copyFileSync(
    new URL(import.meta.resolve(`recheck-parameters/${name}`)),
    new URL(`../${name}`, import.meta.url),
  );
}
