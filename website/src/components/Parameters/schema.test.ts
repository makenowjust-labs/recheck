import { sections, toc } from "./schema";

describe("sections", () => {
  test("follow the schema with stable anchors", () => {
    const runtimeAttackTimeout = sections.find(
      ({ name }) => name === "runtimeAttackTimeout",
    );
    expect(runtimeAttackTimeout).toMatchObject({
      id: "runtimeattacktimeout",
      typescriptType: "number | null",
      scalaType: "Duration",
      unit: "milliseconds",
      typescriptDefault: "-1000",
      scalaDefault: "Duration(-1, SECONDS)",
      cli: "runtime-attack-timeout",
    });
    expect(toc).toContainEqual({
      value: "<code>runtimeAttackTimeout</code>",
      id: "runtimeattacktimeout",
      level: 2,
    });
    expect(toc.map(({ id }) => id)).toEqual(sections.map(({ id }) => id));
  });

  test("show the dialect as a name or an object", () => {
    const dialect = sections.find(({ name }) => name === "dialect");
    expect(dialect).toMatchObject({
      typescriptDefault: '"javascript"',
      scalaDefault: "Dialect.JavaScript",
      cli: "dialect",
      typescriptType: "DialectParameter",
    });
  });

  test("show the types and the defaults of both languages", () => {
    expect(sections.find(({ name }) => name === "checker")).toMatchObject({
      typescriptType: '"auto" | "fuzz" | "automaton"',
      typescriptDefault: '"auto"',
      scalaDefault: "Checker.Auto",
      cli: "checker",
    });
    expect(sections.find(({ name }) => name === "logger")).toMatchObject({
      typescriptType: "(message: string) => void",
      typescriptDefault: "null",
      scalaDefault: "None",
      cli: null,
    });
  });
});
