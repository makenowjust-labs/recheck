import type { Rule } from "eslint";
import type { ParametersFor } from "recheck-parameters";

import { propertiesFor } from "recheck-parameters";

/** Parameters is the recheck parameters that the rule takes as its options. */
export type Parameters = ParametersFor<"eslint-plugin">;

type PropertySchema = Exclude<
  Rule.RuleMetaData["schema"],
  unknown[] | false | undefined
>;

/** The JSON Schema of the options of the parameters. */
export const parameterProperties = Object.fromEntries(
  propertiesFor("eslint-plugin").map(([name, property]) => [
    name,
    property.enum === undefined
      ? { type: property.type }
      : { type: property.type, enum: property.enum },
  ]),
) as Record<keyof Parameters, PropertySchema>;
