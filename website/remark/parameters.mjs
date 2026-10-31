// @ts-check

// A remark plugin to replace the `::parameters` directive with the list of the parameters.
// The list is built from the schema of the parameters on each build, so the headings get into the table of contents.

import { readFileSync } from "node:fs";

import { parse } from "yaml";

/**
 * @typedef {{
 *   title: string;
 *   description: string;
 *   type?: string | string[];
 *   enum?: string[];
 *   default?: unknown;
 *   "x-scala-type": string;
 *   "x-scala-values"?: Record<string, string>;
 *   "x-typescript-type"?: string;
 *   "x-unit"?: string;
 *   "x-cli"?: string;
 *   "x-since"?: string;
 * }} Property
 */

const schemaUrl = new URL(
  "../../modules/recheck-common/parameters.schema.yaml",
  import.meta.url,
);

/** @param {Property} property */
const typescriptType = (property) => {
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

/** @param {Property} property */
const scalaDefault = (property) => {
  const value = property.default ?? null;
  if (property["x-unit"] === "milliseconds") {
    if (value === null) return "Duration.Inf";
    const ms = Number(value);
    return ms % 1000 === 0
      ? `Duration(${ms / 1000}, SECONDS)`
      : `Duration(${ms}, MILLISECONDS)`;
  }
  if (property["x-scala-values"] !== undefined) {
    return property["x-scala-values"][String(value)];
  }
  if (value === null) return "None";
  if (property["x-scala-type"].startsWith("Option[")) {
    return `Some(${JSON.stringify(value)})`;
  }
  return String(value);
};

/**
 * @param {string} name
 * @param {Property} property
 */
const section = (name, property) => {
  const tsDefault = JSON.stringify(property.default ?? null);
  const scala = scalaDefault(property);
  const unit = property["x-unit"] === "milliseconds" ? ", in milliseconds" : "";
  const lines = [
    `## \`${name}\``,
    "",
    `- Type: \`${typescriptType(property)}\` (TypeScript${unit}), \`${property["x-scala-type"]}\` (Scala)`,
    tsDefault === scala
      ? `- Default: \`${tsDefault}\``
      : `- Default: \`${tsDefault}\` (TypeScript), \`${scala}\` (Scala)`,
  ];
  if (property["x-cli"] !== undefined) {
    lines.push(`- CLI: \`--${property["x-cli"]}\``);
  }
  if (property["x-since"] !== undefined) {
    lines.push(`- Since: v${property["x-since"]}`);
  }
  lines.push("", property.description);
  return lines.join("\n");
};

/** Returns the Markdown of the list of the parameters. */
const parametersMarkdown = () => {
  /** @type {{ properties: Record<string, Property> }} */
  const schema = parse(readFileSync(schemaUrl, "utf-8"));
  return Object.entries(schema.properties)
    .map(([name, property]) => section(name, property))
    .join("\n\n");
};

/** @this {any} */
export default function remarkParameters() {
  const processor = this;
  /** @param {any} tree */
  return (tree) => {
    const index = tree.children.findIndex(
      (/** @type {any} */ node) =>
        node.type === "leafDirective" && node.name === "parameters",
    );
    if (index === -1) return;
    const fragment = processor.parse(parametersMarkdown());
    tree.children.splice(index, 1, ...fragment.children);
  };
}
