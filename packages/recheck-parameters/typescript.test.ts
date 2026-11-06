import { generateTypescriptType } from "./typescript.js";
import { schema } from "./index.js";

const { properties } = schema;

const settingsOf = (dialect: string) =>
  properties["dialect"]!.oneOf!.find(
    (variant) => variant.properties?.["name"]?.const === dialect,
  )!.properties!;

describe("generateTypescriptType", () => {
  test("prefers the type that the schema gives", () => {
    expect(generateTypescriptType(properties["dialect"]!)).toBe(
      "DialectParameter",
    );
    expect(generateTypescriptType(properties["logger"]!)).toBe(
      "(message: string) => void",
    );
  });

  test("lists the values of an enum", () => {
    expect(generateTypescriptType(properties["checker"]!)).toBe(
      '"auto" | "fuzz" | "automaton"',
    );
    expect(generateTypescriptType(settingsOf("ruby")["encoding"]!)).toBe(
      '"UTF-8" | "ASCII-8BIT"',
    );
  });

  test("takes a constant as its literal type", () => {
    expect(generateTypescriptType(settingsOf("python")["name"]!)).toBe(
      '"python"',
    );
  });

  test("maps the JSON types to the TypeScript types", () => {
    expect(generateTypescriptType(properties["maxDegree"]!)).toBe("number");
    expect(generateTypescriptType(properties["heatRatio"]!)).toBe("number");
    expect(generateTypescriptType(properties["timeout"]!)).toBe(
      "number | null",
    );
    expect(generateTypescriptType(settingsOf("perl")["utf8"]!)).toBe("boolean");
  });
});
