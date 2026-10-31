import schema from "../../data/parameters.schema.json";

/** Property is a parameter of the schema. */
export type Property = {
  title: string;
  description: string;
  type?: string | string[];
  items?: { type: string };
  enum?: string[];
  default?: unknown;
  "x-scala-type": string;
  "x-scala-values"?: Record<string, string>;
  "x-typescript-type"?: string;
  "x-unit"?: string;
  "x-cli"?: string;
  "x-since"?: string;
  "x-dialects"?: string[];
  "x-encodings"?: Record<string, string[]>;
};

/** Section is what the parameters page shows for a parameter. */
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

const jsonType = (type: string, property: Property): string => {
  if (type === "integer") return "number";
  if (type === "array")
    return `${jsonType(property.items?.type ?? "unknown", property)}[]`;
  return type;
};

export const typescriptType = (property: Property): string => {
  if (property["x-typescript-type"] !== undefined) {
    return property["x-typescript-type"];
  }
  if (property.enum !== undefined) {
    return property.enum.map((value) => JSON.stringify(value)).join(" | ");
  }
  const types = Array.isArray(property.type) ? property.type : [property.type];
  return types.map((type) => jsonType(String(type), property)).join(" | ");
};

export const scalaDefault = (property: Property): string => {
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

/** The sections of all the parameters, in the schema order. */
export const sections: Section[] = Object.entries(
  schema.properties as Record<string, Property>,
).map(([name, property]) => sectionOf(name, property));

/** The table of contents of the sections, which are `h2` headings. */
export const toc: TOCItem[] = sections.map(({ name, id }) => ({
  value: `<code>${name}</code>`,
  id,
  level: 2,
}));
