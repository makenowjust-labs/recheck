const jsonType = (type, property) => {
  if (type === "integer") return "number";
  if (type === "array") {
    return `${jsonType(property.items?.type ?? "unknown", property)}[]`;
  }
  return type;
};

export const typescriptType = (property) => {
  if (property["x-typescript-type"] !== undefined) {
    return property["x-typescript-type"];
  }
  if (property.enum !== undefined) {
    return property.enum.map((value) => JSON.stringify(value)).join(" | ");
  }
  const types = Array.isArray(property.type) ? property.type : [property.type];
  return types.map((type) => jsonType(String(type), property)).join(" | ");
};
