import schema from "./lib/schema.js";

export { schema };
export { generateTypescriptType } from "./typescript.js";

export const defaults = Object.fromEntries(
  Object.entries(schema.properties).map(([name, property]) => [
    name,
    property.default ?? null,
  ]),
);

export const propertiesFor = (target) =>
  Object.entries(schema.properties).filter(
    ([, property]) => !property["x-exclude"]?.includes(target),
  );
