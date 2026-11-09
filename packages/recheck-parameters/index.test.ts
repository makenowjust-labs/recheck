import { defaults, propertiesFor, schema } from "./index.js";

const names = Object.keys(schema.properties);

describe("defaults", () => {
  test("takes the default of each parameter from the schema", () => {
    expect(Object.keys(defaults)).toEqual(names);
    expect(defaults.checker).toBe("auto");
    expect(defaults.dialect).toBe("javascript");
    expect(defaults.timeout).toBe(10000);
    expect(defaults.logger).toBeNull();
  });
});

describe("propertiesFor", () => {
  test("leaves out the parameters that the target excludes, in the schema order", () => {
    expect(propertiesFor("eslint-plugin").map(([name]) => name)).toEqual(
      names.filter(
        (name) =>
          name !== "dialect" && name !== "logger" && name !== "repairTimeout",
      ),
    );
    expect(propertiesFor("playground").map(([name]) => name)).toEqual(
      names.filter((name) => name !== "logger"),
    );
  });

  test("returns the properties of the schema", () => {
    expect(propertiesFor("cli")).toContainEqual([
      "timeout",
      schema.properties["timeout"],
    ]);
  });
});
