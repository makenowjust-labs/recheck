import type { Parameters } from "recheck";
import type { Property } from "recheck-parameters";

import { propertiesFor } from "recheck-parameters";

import { isAtLeast } from "./builds";

import type { Build } from "./builds";

export type ParamType =
  | "duration"
  | "float"
  | "integer"
  | "nullableBoolean"
  | { select: string[] }
  /** A select whose values depend on the dialect, the first value being the default. */
  | { selectByDialect: Record<string, string[]> };

export type Param = {
  title: string;
  name: string;
  defaultValue: string | number;
  type: ParamType;
  /** The first release having the parameter, or `null` if every release has it. */
  since: string | null;
  /** The dialects that the parameter applies to, or `null` if it applies to every dialect. */
  dialects: string[] | null;
};

const paramType = (property: Property): ParamType | null => {
  if (property.enum !== undefined) return { select: property.enum };
  if (property["x-encodings"] !== undefined) {
    return { selectByDialect: property["x-encodings"] };
  }
  if (property["x-unit"] === "milliseconds") return "duration";
  if (property.type === "integer") return "integer";
  if (property.type === "number") return "float";
  const types = Array.isArray(property.type) ? property.type : [];
  if (
    types.length === 2 &&
    types.includes("boolean") &&
    types.includes("null")
  ) {
    return "nullableBoolean";
  }
  return null;
};

/** The values of a nullable boolean in the form. */
export const NULLABLE_BOOLEAN_VALUES = ["null", "true", "false"];

/** The parameters of the form, read from the schema of the parameters. */
const schemaParams: Param[] = propertiesFor("playground").flatMap(
  ([name, property]) => {
    const type = paramType(property);
    if (type === null) {
      return [];
    }
    return [
      {
        name,
        title: property.title,
        defaultValue: (property.default ?? "null") as string | number,
        type,
        since: property["x-since"] ?? null,
        dialects: property["x-dialects"] ?? null,
      },
    ];
  },
);

const loggerParam: Param = {
  name: "logger",
  title: "Logger",
  defaultValue: "off",
  type: { select: ["on", "off"] },
  since: null,
  dialects: null,
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

/** The default dialect, which is the only one of the builds without the `dialect` parameter. */
export const DEFAULT_DIALECT = "javascript";

/** The parameters of the form that the build has and that apply to the dialect. */
export const paramsOf = (
  build: Build,
  dialect: string = DEFAULT_DIALECT,
): Param[] =>
  params.filter(
    ({ since, dialects }) =>
      (since === null || isAtLeast(build, since)) &&
      (dialects === null || dialects.includes(dialect)),
  );

/** The values of a select parameter for the dialect. */
export const selectValues = (param: Param, dialect: string): string[] => {
  const { type } = param;
  if (type === "nullableBoolean") return NULLABLE_BOOLEAN_VALUES;
  if (typeof type !== "object") return [];
  return "select" in type ? type.select : (type.selectByDialect[dialect] ?? []);
};

/** The dialect of the form values, or the default one if the build does not have the `dialect` parameter. */
export const dialectOf = (
  values: Record<string, unknown>,
  build: Build,
): string =>
  paramsOf(build).some(({ name }) => name === "dialect")
    ? String(values["dialect"] ?? DEFAULT_DIALECT)
    : DEFAULT_DIALECT;

// `logger` is a Playground option rather than a `recheck` parameter.
export type CheckParams = Parameters & { logger: "on" | "off" };

/** Converts the form values, where numbers are strings, to the parameters. */
export const parseParams = (
  values: Record<string, unknown>,
  build: Build,
): CheckParams => {
  const parsed: Record<string, unknown> = {};
  const dialect = dialectOf(values, build);
  for (const param of paramsOf(build, dialect)) {
    const { name, type, defaultValue } = param;
    const value = values[name] ?? defaultValue;
    switch (type) {
      case "duration":
      case "integer":
        parsed[name] = Number.parseInt(String(value), 10);
        break;
      case "float":
        parsed[name] = Number.parseFloat(String(value));
        break;
      case "nullableBoolean":
        parsed[name] =
          value === "true" ? true : value === "false" ? false : null;
        break;
      default:
        if (typeof type === "object" && "selectByDialect" in type) {
          // A value left by another dialect falls back to this dialect's default.
          const choices = selectValues(param, dialect);
          parsed[name] = choices.includes(String(value))
            ? String(value)
            : choices[0];
        } else {
          parsed[name] = String(value);
        }
    }
  }
  return parsed as CheckParams;
};

/** Converts the parameters back to the form values, where every value is a string or a number. */
export const formValues = (
  params: Record<string, unknown>,
): Record<string, string | number> =>
  Object.fromEntries(
    Object.entries(params).map(([name, value]) => [
      name,
      typeof value === "number" ? value : String(value),
    ]),
  );

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
