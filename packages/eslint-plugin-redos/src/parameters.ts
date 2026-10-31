import type { Rule } from "eslint";
import type { ParametersFor } from "recheck-parameters";

import { propertiesFor } from "recheck-parameters";

export type Parameters = ParametersFor<"eslint-plugin">;

type PropertySchema = Exclude<
  Rule.RuleMetaData["schema"],
  unknown[] | false | undefined
>;

export const parameterProperties = Object.fromEntries(
  propertiesFor("eslint-plugin").map(([name, property]) => [
    name,
    property.enum === undefined
      ? { type: property.type }
      : { type: property.type, enum: property.enum },
  ]),
) as Record<keyof Parameters, PropertySchema>;
