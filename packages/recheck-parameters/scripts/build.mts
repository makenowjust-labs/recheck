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

// The variants of `dialect` give the diagnostics' `Dialect` with every setting, and the parameter with optional ones.
const dialectDts = (dialect: Property): string[] => {
  const variants = (dialect.oneOf ?? []).filter(
    (variant) => variant.type === "object",
  );
  const union = (required: boolean): string =>
    variants
      .map((variant) => {
        const fields = Object.entries(variant.properties ?? {}).map(
          ([key, setting]) =>
            `${key}${required || key === "name" ? "" : "?"}: ${typescriptType(setting)}`,
        );
        return `\n  | { ${fields.join("; ")} }`;
      })
      .join("");
  return [
    comment(
      [
        "Dialect is the dialect of a diagnostics as the parameter gives it, with every setting of the dialect.",
        "An omitted setting is the default.",
      ],
      "",
    ),
    `export type Dialect =${union(true)};`,
    "",
    comment(
      [
        "DialectSettings is the name and some settings of a dialect, where an omitted setting is the default.",
      ],
      "",
    ),
    `export type DialectSettings =${union(false)};`,
    "",
    comment(
      [
        "DialectParameter is the `dialect` parameter: the name of a dialect, or its name and some settings.",
      ],
      "",
    ),
    'export type DialectParameter = Dialect["name"] | DialectSettings;',
    "",
  ];
};

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
    ...dialectDts(schema.properties["dialect"]!),
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
