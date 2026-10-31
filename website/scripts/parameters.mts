// Converts the schema of the parameters to JSON for the parameters page and the Playground form.
// It also generates the `Parameters` type of `recheck`, which this site's type check needs.

import { mkdirSync, readFileSync, writeFileSync } from "node:fs";

import { parse } from "yaml";

const schemaUrl = new URL(
  "../../modules/recheck-common/parameters.schema.yaml",
  import.meta.url,
);
const jsonUrl = new URL("../src/data/parameters.schema.json", import.meta.url);
const recheckGeneratorUrl = new URL(
  "../../packages/recheck/scripts/parameters.mts",
  import.meta.url,
);

await import(recheckGeneratorUrl.href);

const schema = parse(readFileSync(schemaUrl, "utf-8"));
mkdirSync(new URL(".", jsonUrl), { recursive: true });
writeFileSync(jsonUrl, `${JSON.stringify(schema, null, 2)}\n`);
