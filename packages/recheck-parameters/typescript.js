export const generateTypescriptType = (property) => {
  if (property["x-typescript-type"] !== undefined) {
    return property["x-typescript-type"];
  }
  if (property.const !== undefined) return JSON.stringify(property.const);
  if (property.enum !== undefined) {
    return property.enum.map((value) => JSON.stringify(value)).join(" | ");
  }
  const types = Array.isArray(property.type) ? property.type : [property.type];
  return types
    .map((type) => (type === "integer" ? "number" : String(type)))
    .join(" | ");
};
