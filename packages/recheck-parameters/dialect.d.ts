/**
 * Dialect is the dialect of a diagnostics as the parameter gives it, with every setting of the dialect.
 * An omitted setting is the default.
 */
export type Dialect =
  | { name: "javascript" }
  | { name: "java" }
  | { name: "dotnet" }
  | { name: "pcre" }
  | { name: "python"; type: "str" | "bytes" }
  | { name: "ruby"; encoding: "UTF-8" | "ASCII-8BIT" }
  | { name: "perl"; utf8: boolean; source: "literal" | "string" };

/**
 * DialectSettings is the name and some settings of a dialect, where an omitted setting is the default.
 */
export type DialectSettings = {
  [N in Dialect["name"]]: { name: N } & Partial<
    Omit<Extract<Dialect, { name: N }>, "name">
  >;
}[Dialect["name"]];

/**
 * DialectParameter is the `dialect` parameter: the name of a dialect, or its name and some settings.
 */
export type DialectParameter = Dialect["name"] | DialectSettings;
