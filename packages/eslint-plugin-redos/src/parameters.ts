import type { Rule } from "eslint";
import type { ParametersFor, Property } from "recheck-parameters";

import { propertiesFor } from "recheck-parameters";

export type Parameters = ParametersFor<"eslint-plugin">;

type PropertySchema = Exclude<
  Rule.RuleMetaData["schema"],
  unknown[] | false | undefined
>;

const schemaOf = (property: Property) => {
  if (property.enum !== undefined) {
    return { type: property.type, enum: property.enum };
  }
  if (property.properties !== undefined) {
    return {
      type: property.type,
      properties: property.properties,
      additionalProperties: false,
    };
  }
  return { type: property.type };
};

export const parameterProperties = Object.fromEntries(
  propertiesFor("eslint-plugin").map(([name, property]) => [
    name,
    schemaOf(property),
  ]),
) as Record<keyof Parameters, PropertySchema>;
