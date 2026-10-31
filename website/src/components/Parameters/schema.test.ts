import { parseMarkdown } from "./markdown";
import { sectionOf, sections, toc } from "./schema";

describe("sections", () => {
  test("follow the schema with stable anchors", () => {
    const recallTimeout = sections.find(({ name }) => name === "recallTimeout");
    expect(recallTimeout).toMatchObject({
      id: "recalltimeout",
      typescriptType: "number | null",
      scalaType: "Duration",
      unit: "milliseconds",
      typescriptDefault: "-1000",
      scalaDefault: "Duration(-1, SECONDS)",
      cli: "recall-timeout",
    });
    expect(toc).toContainEqual({
      value: "<code>recallTimeout</code>",
      id: "recalltimeout",
      level: 2,
    });
    expect(toc.map(({ id }) => id)).toEqual(sections.map(({ id }) => id));
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
    expect(
      sectionOf("command", {
        title: "Command",
        description: "",
        type: ["array", "null"],
        items: { type: "string" },
        default: null,
        "x-scala-type": "Option[Seq[String]]",
      }),
    ).toMatchObject({
      typescriptType: "string[] | null",
      scalaDefault: "None",
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
