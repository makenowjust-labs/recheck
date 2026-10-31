import type { Parameters } from "./lib/parameters.js";
import type { Defaults, Excluded } from "./lib/types.js";

export type { Defaults, Parameters };

/** Target is a consumer of the parameters that may leave some of them out by `x-exclude`. */
export type Target = keyof Excluded;

/** ParametersFor is the parameters that the target exposes. */
export type ParametersFor<T extends Target> = Omit<Parameters, Excluded[T]>;

/** Property is a parameter of the schema, with the annotations described in the schema. */
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
  "x-cli-metavar"?: string;
  "x-exclude"?: string[];
  "x-since"?: string;
  "x-dialects"?: string[];
  "x-encodings"?: Record<string, string[]>;
};

/** Schema is the JSON Schema of the parameters. */
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

/** Returns the parameters that the target exposes, in the schema order. */
export declare const propertiesFor: <T extends Target>(
  target: T,
) => [keyof ParametersFor<T> & string, Property][];

export { typescriptType } from "./typescript.js";
