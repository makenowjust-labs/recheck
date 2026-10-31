// A Markdown reader for the descriptions of the schema of the parameters.
// It reads only what the descriptions use: paragraphs, bullet lists, inline code and links.

export type Inline =
  | { type: "text"; value: string }
  | { type: "code"; value: string }
  | { type: "link"; href: string; children: Inline[] };

export type Block =
  | { type: "paragraph"; children: Inline[] }
  | { type: "list"; items: Inline[][] };

const INLINE = /`([^`]+)`|\[([^\]]+)\]\(([^)\s]+)\)/g;

export const parseInline = (text: string): Inline[] => {
  const inlines: Inline[] = [];
  let last = 0;
  for (const match of text.matchAll(INLINE)) {
    if (match.index > last) {
      inlines.push({ type: "text", value: text.slice(last, match.index) });
    }
    if (match[1] !== undefined) {
      inlines.push({ type: "code", value: match[1] });
    } else {
      inlines.push({
        type: "link",
        href: match[3]!,
        children: parseInline(match[2]!),
      });
    }
    last = match.index + match[0].length;
  }
  if (last < text.length) {
    inlines.push({ type: "text", value: text.slice(last) });
  }
  return inlines;
};

export const parseMarkdown = (markdown: string): Block[] => {
  // Each block is a list of lines: a paragraph's lines, or a list's items.
  const blocks: { type: "paragraph" | "list"; lines: string[] }[] = [];
  let current: (typeof blocks)[number] | null = null;
  for (const line of markdown.split("\n")) {
    if (line.trim() === "") {
      current = null;
    } else if (line.startsWith("- ")) {
      if (current?.type !== "list") {
        current = { type: "list", lines: [] };
        blocks.push(current);
      }
      current.lines.push(line.slice(2));
    } else if (current?.type === "list" && line.startsWith("  ")) {
      current.lines[current.lines.length - 1] += ` ${line.trim()}`;
    } else {
      if (current?.type !== "paragraph") {
        current = { type: "paragraph", lines: [] };
        blocks.push(current);
      }
      current.lines.push(line.trim());
    }
  }
  return blocks.map(({ type, lines }) =>
    type === "list"
      ? { type, items: lines.map(parseInline) }
      : { type, children: parseInline(lines.join(" ")) },
  );
};
