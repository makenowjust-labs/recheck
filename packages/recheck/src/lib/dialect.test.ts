import { unparsedDialect } from "./dialect";

test("unparsedDialect", () => {
  expect(unparsedDialect("a")).toEqual({ name: "javascript" });
  expect(unparsedDialect("a", "dotnet")).toEqual({ name: "dotnet" });
  expect(unparsedDialect("a", { name: "java" })).toEqual({ name: "java" });
  expect(unparsedDialect("a", "python")).toEqual({
    name: "python",
    encoding: "str",
  });
  expect(
    unparsedDialect("a", { name: "ruby", encoding: "ASCII-8BIT" }),
  ).toEqual({ name: "ruby", encoding: "ASCII-8BIT" });
  expect(unparsedDialect("a", "perl")).toEqual({
    name: "perl",
    encoding: "characters",
    source: "literal",
    utf8: false,
  });
  expect(unparsedDialect("é", { name: "perl", source: "string" })).toEqual({
    name: "perl",
    encoding: "characters",
    source: "string",
    utf8: true,
  });
  expect(
    unparsedDialect("é", { name: "perl", encoding: "bytes" }),
  ).toMatchObject({ utf8: false });
  expect(unparsedDialect("a", { name: "perl", utf8: true })).toMatchObject({
    utf8: true,
  });
});
