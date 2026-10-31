// Reads the schema of the parameters and writes `lib/`: the schema as a module and the types generated from it.
// `lib/parameters.d.ts` is self-contained because `recheck` publishes a copy of it.

import { mkdirSync, readFileSync, writeFileSync } from "node:fs";

import { parse } from "yaml";

import { typescriptType } from "../typescript.js";

import type { Property, Schema } from "../index.js";

const schemaUrl = new URL(
  "../../../modules/recheck-common/parameters.schema.yaml",
  import.meta.url,
);
const libUrl = new URL("../lib/", import.meta.url);

const header =
  "// This file is generated from `modules/recheck-common/parameters.schema.yaml` by `recheck-parameters`.";

const comment = (lines: string[], indent: string): string =>
  [
    `${indent}/**`,
    ...lines.map((line) =>
      line === "" ? `${indent} *` : `${indent} * ${line}`,
    ),
    `${indent} */`,
  ].join("\n");

const defaultOf = (property: Property): string =>
  JSON.stringify(property.default ?? null);

const parametersDts = (schema: Schema): string => {
  const fields = Object.entries(schema.properties).map(([name, property]) => {
    const lines = [
      ...property.description.split("\n"),
      "",
      `(default: \`${defaultOf(property)}\`)`,
    ];
    if (property["x-since"] !== undefined) {
      lines.push("", `@since ${property["x-since"]}`);
    }
    return `${comment(lines, "  ")}\n  ${name}?: ${typescriptType(property)};`;
  });
  return [
    header,
    "",
    comment([schema.description], ""),
    "export type Parameters = {",
    fields.join("\n\n"),
    "};",
    "",
  ].join("\n");
};

const typesDts = (schema: Schema): string => {
  const entries = Object.entries(schema.properties);
  const targets = [
    ...new Set(entries.flatMap(([, property]) => property["x-exclude"] ?? [])),
  ].sort();
  const excluded = targets.map((target) => {
    const names = entries
      .filter(([, property]) => property["x-exclude"]?.includes(target))
      .map(([name]) => JSON.stringify(name));
    return `  ${JSON.stringify(target)}: ${names.join(" | ")};`;
  });
  const defaults = entries.map(
    ([name, property]) => `  ${name}: ${defaultOf(property)};`,
  );
  return [
    header,
    "",
    "/** The names of the parameters that each target leaves out. */",
    "export type Excluded = {",
    ...excluded,
    "};",
    "",
    "/** The default value of each parameter. */",
    "export type Defaults = {",
    ...defaults,
    "};",
    "",
  ].join("\n");
};

const schema = parse(readFileSync(schemaUrl, "utf-8")) as Schema;

mkdirSync(libUrl, { recursive: true });
writeFileSync(
  new URL("schema.js", libUrl),
  `${header}\n\nexport default ${JSON.stringify(schema, null, 2)};\n`,
);
writeFileSync(new URL("parameters.d.ts", libUrl), parametersDts(schema));
writeFileSync(new URL("types.d.ts", libUrl), typesDts(schema));
