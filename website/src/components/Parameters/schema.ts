import type { Property } from "recheck-parameters";

import { schema, typescriptType } from "recheck-parameters";

export type Section = {
  name: string;
  /** The anchor of the heading, which is the slug of the name as for a Markdown heading. */
  id: string;
  typescriptType: string;
  scalaType: string;
  unit: string | null;
  typescriptDefault: string;
  scalaDefault: string;
  cli: string | null;
  since: string | null;
  description: string;
};

/** TOCItem is an item of a page's table of contents, as Docusaurus takes it. */
export type TOCItem = { value: string; id: string; level: number };

export const scalaDefault = (property: Property): string => {
  if (property["x-scala-default"] !== undefined) {
    return property["x-scala-default"];
  }
  const value = property.default ?? null;
  if (property["x-unit"] === "milliseconds") {
    if (value === null) return "Duration.Inf";
    const ms = Number(value);
    return ms % 1000 === 0
      ? `Duration(${ms / 1000}, SECONDS)`
      : `Duration(${ms}, MILLISECONDS)`;
  }
  if (property["x-scala-values"] !== undefined) {
    return property["x-scala-values"][String(value)]!;
  }
  if (value === null) return "None";
  if (property["x-scala-type"].startsWith("Option[")) {
    return `Some(${JSON.stringify(value)})`;
  }
  return String(value);
};

export const sectionOf = (name: string, property: Property): Section => ({
  name,
  id: name.toLowerCase(),
  typescriptType: typescriptType(property),
  scalaType: property["x-scala-type"],
  unit: property["x-unit"] ?? null,
  typescriptDefault: JSON.stringify(property.default ?? null),
  scalaDefault: scalaDefault(property),
  cli: property["x-cli"] ?? null,
  since: property["x-since"] ?? null,
  description: property.description,
});

export const sections: Section[] = Object.entries(schema.properties).map(
  ([name, property]) => sectionOf(name, property),
);

export const toc: TOCItem[] = sections.map(({ name, id }) => ({
  value: `<code>${name}</code>`,
  id,
  level: 2,
}));
