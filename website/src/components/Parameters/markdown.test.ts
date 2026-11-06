import { parseMarkdown } from "./markdown";

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
