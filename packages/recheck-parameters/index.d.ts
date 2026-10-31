import type { Parameters } from "./lib/parameters.js";
import type { Defaults, Excluded } from "./lib/types.js";

export type { Defaults, Parameters };

/** Target is a consumer of the parameters that may leave some of them out by `x-exclude`. */
export type Target = keyof Excluded;

export type ParametersFor<T extends Target> = Omit<Parameters, Excluded[T]>;

/** Variant is an alternative of `oneOf` of a parameter: an enum of names, or an object of a name and its settings. */
export type Variant = {
  type: "string" | "object";
  enum?: string[];
  properties?: Record<string, Setting>;
  required?: string[];
  additionalProperties?: boolean;
};

/** Setting is a property of a variant object: `name` with `const`, or a setting with `enum` or `type`. */
export type Setting = {
  title?: string;
  const?: string;
  type?: string | string[];
  enum?: string[];
  default?: unknown;
};

export type Property = {
  title: string;
  description: string;
  type?: string | string[];
  items?: { type: string };
  enum?: string[];
  oneOf?: Variant[];
  default?: unknown;
  "x-scala-type": string;
  "x-scala-values"?: Record<string, string>;
  "x-scala-default"?: string;
  "x-typescript-type"?: string;
  "x-unit"?: string;
  "x-cli"?: string;
  "x-cli-metavar"?: string;
  "x-exclude"?: string[];
  "x-since"?: string;
};

export type Schema = {
  $schema: string;
  $id: string;
  title: string;
  description: string;
  type: "object";
  additionalProperties: false;
  properties: Record<string, Property>;
};

/** The schema of the parameters, read from `modules/recheck-common/parameters.schema.yaml` at build time. */
export declare const schema: Schema;

/** The default value of each parameter, which is `null` if the schema has none. */
export declare const defaults: Defaults;

/** The entries are in the schema order. */
export declare const propertiesFor: <T extends Target>(
  target: T,
) => [keyof ParametersFor<T> & string, Property][];

export { typescriptType } from "./typescript.js";
