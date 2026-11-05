import { parseMarkdown } from "./markdown";
import { sectionOf, sections, toc } from "./schema";

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
    expect(
      sectionOf("checker", {
        title: "Checker",
        description: "",
        type: "string",
        enum: ["auto", "fuzz"],
        default: "auto",
        "x-scala-type": "Checker",
        "x-scala-values": { auto: "Checker.Auto", fuzz: "Checker.Fuzz" },
      }),
    ).toMatchObject({
      typescriptType: '"auto" | "fuzz"',
      typescriptDefault: '"auto"',
      scalaDefault: "Checker.Auto",
      cli: null,
      since: null,
    });
    expect(sections.find(({ name }) => name === "logger")).toMatchObject({
      typescriptType: "(message: string) => void",
      typescriptDefault: "null",
      scalaDefault: "None",
      cli: null,
    });
  });
});

describe("parseMarkdown", () => {
  test("reads paragraphs, lists, code and links", () => {
    expect(
      parseMarkdown(
        "The `a`\nand [the `b`](b.md).\n\n- `x` is\n  long.\n- y\n\nEnd.",
      ),
    ).toEqual([
      {
        type: "paragraph",
        children: [
          { type: "text", value: "The " },
          { type: "code", value: "a" },
          { type: "text", value: " and " },
          {
            type: "link",
            href: "b.md",
            children: [
              { type: "text", value: "the " },
              { type: "code", value: "b" },
            ],
          },
          { type: "text", value: "." },
        ],
      },
      {
        type: "list",
        items: [
          [
            { type: "code", value: "x" },
            { type: "text", value: " is long." },
          ],
          [{ type: "text", value: "y" }],
        ],
      },
      { type: "paragraph", children: [{ type: "text", value: "End." }] },
    ]);
  });
});
