const jsonType = (type, property) => {
  if (type === "integer") return "number";
  if (type === "array") {
    return `${jsonType(property.items?.type ?? "unknown", property)}[]`;
  }
  return type;
};

const objectType = (property) => {
  const required = property.required ?? [];
  const fields = Object.entries(property.properties ?? {}).map(
    ([key, value]) =>
      `${key}${required.includes(key) ? "" : "?"}: ${typescriptType(value)}`,
  );
  return `{ ${fields.join("; ")} }`;
};

export const typescriptType = (property) => {
  if (property["x-typescript-type"] !== undefined) {
    return property["x-typescript-type"];
  }
  if (property.oneOf !== undefined) {
    return property.oneOf.map(typescriptType).join(" | ");
  }
  if (property.const !== undefined) return JSON.stringify(property.const);
  if (property.type === "object") return objectType(property);
  if (property.enum !== undefined) {
    return property.enum.map((value) => JSON.stringify(value)).join(" | ");
  }
  const types = Array.isArray(property.type) ? property.type : [property.type];
  return types.map((type) => jsonType(String(type), property)).join(" | ");
};
