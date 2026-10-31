import type { Dialect, Parameters } from "../..";

/**
 * Returns the dialect of a diagnostics for the source that is not parsed, with every setting.
 * It follows `ReDoS.unparsedDialect` of the Scala library: Perl's `utf8` is decided from the source, where a byte string is not UTF-8.
 */
export const unparsedDialect = (
  source: string,
  dialect: Parameters["dialect"] = "javascript",
): Dialect => {
  // A name is the object of the name without the settings.
  const settings =
    typeof dialect === "string"
      ? ({ name: dialect } as Exclude<
          Parameters["dialect"],
          string | undefined
        >)
      : dialect;
  switch (settings.name) {
    case "python":
      return { name: "python", encoding: settings.encoding ?? "str" };
    case "ruby":
      return { name: "ruby", encoding: settings.encoding ?? "UTF-8" };
    case "perl": {
      const encoding = settings.encoding ?? "characters";
      return {
        name: "perl",
        encoding,
        source: settings.source ?? "literal",
        utf8:
          settings.utf8 ?? (encoding !== "bytes" && /[^\0-\x7f]/.test(source)),
      };
    }
    default:
      return { name: settings.name };
  }
};
