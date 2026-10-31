// Generates `src/parameters.ts`, the rule options of the recheck parameters, from the schema of the parameters.
// The parameters excluded for `eslint-plugin` (the logger and the dialect ones) are not options of the rule.

import { readFileSync, writeFileSync } from "node:fs";

import { parse } from "yaml";

type Property = {
  type?: string | string[];
  enum?: string[];
  default?: unknown;
  "x-exclude"?: string[];
};

type Schema = {
  properties: Record<string, Property>;
};

const schemaUrl = new URL(
  "../../../modules/recheck-common/parameters.schema.yaml",
  import.meta.url,
);
const outputUrl = new URL("../src/parameters.ts", import.meta.url);

const generate = (yaml: string): string => {
  const schema = parse(yaml) as Schema;
  const entries = Object.entries(schema.properties);
  const excluded = entries
    .filter(([, property]) => property["x-exclude"]?.includes("eslint-plugin"))
    .map(([name]) => name);
  const included = entries.filter(([name]) => !excluded.includes(name));
  const properties = Object.fromEntries(
    included.map(([name, property]) => [
      name,
      {
        type: property.type,
        ...(property.enum === undefined ? {} : { enum: property.enum }),
      },
    ]),
  );
  const defaults = Object.fromEntries(
    included.map(([name, property]) => [name, property.default]),
  );
  return [
    "// This file is generated from `modules/recheck-common/parameters.schema.yaml` by `scripts/parameters.mts`.",
    "",
    'import type { Rule } from "eslint";',
    'import type * as ReDoS from "recheck";',
    "",
    "/** Parameters is the recheck parameters that the rule takes as its options. */",
    `export type Parameters = Omit<ReDoS.Parameters, ${excluded.map((name) => JSON.stringify(name)).join(" | ")}>;`,
    "",
    "/** The JSON Schema of the options of the parameters. */",
    `export const parameterProperties: Record<keyof Parameters, Exclude<Rule.RuleMetaData["schema"], unknown[] | false | undefined>> = ${JSON.stringify(properties, null, 2)};`,
    "",
    "/** The default values of the parameters. */",
    `export const parameterDefaults = ${JSON.stringify(defaults, null, 2)} as const;`,
    "",
  ].join("\n");
};

writeFileSync(outputUrl, generate(readFileSync(schemaUrl, "utf-8")));
