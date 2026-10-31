import type { Parameters } from "recheck";
import type { Property, Setting } from "recheck-parameters";

import { propertiesFor } from "recheck-parameters";

import { isAtLeast } from "./builds";

import type { Build } from "./builds";

export type ParamType =
  | "duration"
  | "float"
  | "integer"
  | "boolean"
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
  /** The key of a setting of the dialect, which goes into the `dialect` object, or `null` for a parameter. */
  setting: string | null;
};

const paramType = (property: Property): ParamType | null => {
  if (property.enum !== undefined) return { select: property.enum };
  // `dialect` is the name of the language or an object of the name and the settings.
  const names = property.oneOf?.find(({ type }) => type === "string")?.enum;
  if (names !== undefined) return { select: names };
  if (property["x-unit"] === "milliseconds") return "duration";
  if (property.type === "integer") return "integer";
  if (property.type === "number") return "float";
  if (property.type === "boolean") return "boolean";
  return null;
};

const settingName = (key: string): string =>
  `dialect${key[0]!.toUpperCase()}${key.slice(1)}`;

/** The settings of the dialects in the `oneOf` of `dialect`, as the parameters of the form. */
const settingParams = (property: Property): Param[] => {
  const settings = new Map<string, [string, Setting][]>();
  for (const variant of property.oneOf ?? []) {
    const { name, ...rest } = variant.properties ?? {};
    if (name?.const === undefined) continue;
    for (const [key, setting] of Object.entries(rest)) {
      settings.set(key, [...(settings.get(key) ?? []), [name.const, setting]]);
    }
  }
  return [...settings].flatMap(([key, entries]) => {
    const [, first] = entries[0]!;
    const type: ParamType | null =
      first.type === "boolean"
        ? "boolean"
        : first.enum !== undefined
          ? {
              selectByDialect: Object.fromEntries(
                entries.map(([dialect, { enum: values = [], default: d }]) => [
                  dialect,
                  [String(d), ...values.filter((v) => v !== d)],
                ]),
              ),
            }
          : null;
    if (type === null) return [];
    return [
      {
        name: settingName(key),
        title: first.title ?? key,
        defaultValue: String(first.default),
        type,
        since: property["x-since"] ?? null,
        dialects: entries.map(([dialect]) => dialect),
        setting: key,
      },
    ];
  });
};

export const BOOLEAN_VALUES = ["true", "false"];

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
        dialects: null,
        setting: null,
      },
      ...(name === "dialect" ? settingParams(property) : []),
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
  setting: null,
};

/** The parameters shown first, in this order; the others follow in the schema order. */
const FIRST = ["checker", "logger", "timeout", "dialect"];

// The settings of the dialect follow `dialect`.
const order = ({ name, setting }: Param): number => {
  const index = FIRST.indexOf(setting === null ? name : "dialect");
  return index === -1 ? FIRST.length : index;
};

/** All the parameters of the form, for the bundled build. */
export const params: Param[] = [...schemaParams, loggerParam]
  .map((param, index) => ({ param, index }))
  .sort((a, b) => order(a.param) - order(b.param) || a.index - b.index)
  .map(({ param }) => param);

/** The default dialect, which is the only one of the builds without the `dialect` parameter. */
export const DEFAULT_DIALECT = "javascript";

export const paramsOf = (
  build: Build,
  dialect: string = DEFAULT_DIALECT,
): Param[] =>
  params.filter(
    ({ since, dialects }) =>
      (since === null || isAtLeast(build, since)) &&
      (dialects === null || dialects.includes(dialect)),
  );

export const selectValues = (param: Param, dialect: string): string[] => {
  const { type } = param;
  if (type === "boolean") return BOOLEAN_VALUES;
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
  const settings: Record<string, unknown> = {};
  const dialect = dialectOf(values, build);
  for (const param of paramsOf(build, dialect)) {
    const { name, defaultValue, setting } = param;
    const value = values[name] ?? defaultValue;
    if (setting !== null) {
      settings[setting] = parseValue(param, value, dialect);
      continue;
    }
    parsed[name] = parseValue(param, value, dialect);
  }
  if ("dialect" in parsed) parsed["dialect"] = { name: dialect, ...settings };
  return parsed as CheckParams;
};

const parseValue = (param: Param, value: unknown, dialect: string): unknown => {
  const { type } = param;
  switch (type) {
    case "duration":
    case "integer":
      return Number.parseInt(String(value), 10);
    case "float":
      return Number.parseFloat(String(value));
    case "boolean":
      return String(value) === "true";
    default:
      if (typeof type === "object" && "selectByDialect" in type) {
        // A value left by another dialect falls back to this dialect's default.
        const choices = selectValues(param, dialect);
        return choices.includes(String(value)) ? String(value) : choices[0];
      }
      return String(value);
  }
};

export const formValues = (
  params: Record<string, unknown>,
): Record<string, string | number> =>
  Object.fromEntries(
    Object.entries(params).flatMap(
      ([name, value]): [string, string | number][] => {
        if (name === "dialect" && typeof value === "object" && value !== null) {
          const { name: dialect, ...settings } = value as Record<
            string,
            unknown
          >;
          return [
            ["dialect", String(dialect)],
            ...Object.entries(settings).map(
              ([key, setting]): [string, string] => [
                settingName(key),
                String(setting),
              ],
            ),
          ];
        }
        return [[name, typeof value === "number" ? value : String(value)]];
      },
    ),
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
