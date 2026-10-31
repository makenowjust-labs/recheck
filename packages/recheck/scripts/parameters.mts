// Generates `parameters.d.ts`, the `Parameters` type, from the schema of the parameters.

import { readFileSync, writeFileSync } from "node:fs";

import { parse } from "yaml";

type Property = {
  description: string;
  type?: string | string[];
  enum?: string[];
  default?: unknown;
  "x-typescript-type"?: string;
  "x-since"?: string;
};

type Schema = {
  description: string;
  properties: Record<string, Property>;
};

const schemaUrl = new URL(
  "../../../modules/recheck-common/parameters.schema.yaml",
  import.meta.url,
);
const outputUrl = new URL("../parameters.d.ts", import.meta.url);

const typeOf = (property: Property): string => {
  if (property["x-typescript-type"] !== undefined) {
    return property["x-typescript-type"];
  }
  if (property.enum !== undefined) {
    return property.enum.map((value) => JSON.stringify(value)).join(" | ");
  }
  const types = Array.isArray(property.type) ? property.type : [property.type];
  return types
    .map((type) => (type === "integer" ? "number" : String(type)))
    .join(" | ");
};

const comment = (lines: string[], indent: string): string =>
  [
    `${indent}/**`,
    ...lines.map((line) =>
      line === "" ? `${indent} *` : `${indent} * ${line}`,
    ),
    `${indent} */`,
  ].join("\n");

const generate = (yaml: string): string => {
  const schema = parse(yaml) as Schema;
  const fields = Object.entries(schema.properties).map(([name, property]) => {
    const lines = [
      ...property.description.split("\n"),
      "",
      `(default: \`${JSON.stringify(property.default ?? null)}\`)`,
    ];
    if (property["x-since"] !== undefined) {
      lines.push("", `@since ${property["x-since"]}`);
    }
    return `${comment(lines, "  ")}\n  ${name}?: ${typeOf(property)};`;
  });
  return [
    "// This file is generated from `modules/recheck-common/parameters.schema.yaml` by `scripts/parameters.mts`.",
    "",
    comment([schema.description], ""),
    "export type Parameters = {",
    fields.join("\n\n"),
    "};",
    "",
  ].join("\n");
};

writeFileSync(outputUrl, generate(readFileSync(schemaUrl, "utf-8")));
