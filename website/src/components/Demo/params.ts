import type { Parameters } from "recheck";

import { isAtLeast } from "./builds";
import schema from "./parameters.schema.json";

import type { Build } from "./builds";

export type ParamType = "duration" | "float" | "integer" | { select: string[] };

export type Param = {
  title: string;
  name: string;
  defaultValue: string | number;
  type: ParamType;
  /** The first release having the parameter, or `null` if every release has it. */
  since: string | null;
};

type Property = {
  title: string;
  type?: string | string[];
  enum?: string[];
  default?: unknown;
  "x-unit"?: string;
  "x-exclude"?: string[];
  "x-since"?: string;
};

const paramType = (property: Property): ParamType | null => {
  if (property.enum !== undefined) return { select: property.enum };
  if (property["x-unit"] === "milliseconds") return "duration";
  if (property.type === "integer") return "integer";
  if (property.type === "number") return "float";
  return null;
};

/** The parameters of the form, read from the schema of the parameters. */
const schemaParams: Param[] = Object.entries(
  schema.properties as Record<string, Property>,
).flatMap(([name, property]) => {
  const type = paramType(property);
  if (property["x-exclude"]?.includes("playground") || type === null) {
    return [];
  }
  return [
    {
      name,
      title: property.title,
      defaultValue: property.default as string | number,
      type,
      since: property["x-since"] ?? null,
    },
  ];
});

const loggerParam: Param = {
  name: "logger",
  title: "Logger",
  defaultValue: "off",
  type: { select: ["on", "off"] },
  since: null,
};

/** The parameters shown first, in this order; the others follow in the schema order. */
const FIRST = ["checker", "logger", "timeout", "dialect"];

const order = (name: string): number => {
  const index = FIRST.indexOf(name);
  return index === -1 ? FIRST.length : index;
};

/** All the parameters of the form, for the bundled build. */
export const params: Param[] = [...schemaParams, loggerParam]
  .map((param, index) => ({ param, index }))
  .sort(
    (a, b) => order(a.param.name) - order(b.param.name) || a.index - b.index,
  )
  .map(({ param }) => param);

/** The parameters of the form that the build has. */
export const paramsOf = (build: Build): Param[] =>
  params.filter(({ since }) => since === null || isAtLeast(build, since));

// `logger` is a Playground option rather than a `recheck` parameter.
export type CheckParams = Parameters & { logger: "on" | "off" };

/** Converts the form values, where numbers are strings, to the parameters. */
export const parseParams = (
  values: Record<string, unknown>,
  build: Build,
): CheckParams => {
  const parsed: Record<string, unknown> = {};
  for (const { name, type, defaultValue } of paramsOf(build)) {
    const value = values[name] ?? defaultValue;
    switch (type) {
      case "duration":
      case "integer":
        parsed[name] = Number.parseInt(String(value), 10);
        break;
      case "float":
        parsed[name] = Number.parseFloat(String(value));
        break;
      default:
        parsed[name] = String(value);
    }
  }
  return parsed as CheckParams;
};

export const extract = (
  input: string,
): { source: string; flags: string } | null => {
  if (!input.startsWith("/")) return null;

  const lastSlashPos = input.lastIndexOf("/");
  if (lastSlashPos === 0) return null;

  return {
    source: input.slice(1, lastSlashPos),
    flags: input.slice(lastSlashPos + 1),
  };
};
