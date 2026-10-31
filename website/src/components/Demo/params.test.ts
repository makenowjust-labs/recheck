import {
  extract,
  formValues,
  params,
  paramsOf,
  parseParams,
  selectValues,
} from "./params";

import type { Build } from "./builds";

const dev: Build = { type: "dev" };

describe("parseParams", () => {
  test("falls back to the default values", () => {
    const parsed = parseParams({}, dev);
    expect(parsed.checker).toBe("auto");
    expect(parsed.logger).toBe("off");
    expect(parsed.timeout).toBe(10000);
    expect(parsed.heatRatio).toBe(0.001);
    expect(parsed.recallTimeout).toBe(-1000);
    expect(parsed.dialect).toEqual({ name: "javascript" });
    expect(Object.keys(parsed).sort()).toEqual(
      paramsOf(dev)
        .map(({ name }) => name)
        .sort(),
    );
  });

  test("parses the form values as numbers by type", () => {
    const parsed = parseParams(
      {
        checker: "fuzz",
        logger: "on",
        timeout: "3000",
        heatRatio: "0.5",
        maxDegree: "2",
      },
      dev,
    );
    expect(parsed.checker).toBe("fuzz");
    expect(parsed.logger).toBe("on");
    expect(parsed.timeout).toBe(3000);
    expect(parsed.heatRatio).toBe(0.5);
    expect(parsed.maxDegree).toBe(2);
  });

  test("parses the settings of the dialect into the dialect", () => {
    expect(parseParams({ dialectUtf8: "true" }, dev).dialect).toEqual({
      name: "javascript",
    });
    expect(
      parseParams(
        { dialect: "perl", dialectEncoding: "bytes", dialectUtf8: "true" },
        dev,
      ).dialect,
    ).toEqual({
      name: "perl",
      encoding: "bytes",
      source: "literal",
      utf8: true,
    });
    expect(
      parseParams({ dialect: "perl", dialectUtf8: "false" }, dev).dialect,
    ).toMatchObject({ utf8: false });
    expect(parseParams({ dialect: "perl" }, dev).dialect).toMatchObject({
      utf8: null,
    });
    // An encoding of another dialect falls back to the default.
    expect(
      parseParams({ dialect: "ruby", dialectEncoding: "str" }, dev).dialect,
    ).toEqual({ name: "ruby", encoding: "UTF-8" });
    expect(parseParams({ dialect: "python" }, dev).dialect).toEqual({
      name: "python",
      encoding: "str",
    });
    expect(parseParams({ dialect: "python" }, dev)).not.toHaveProperty(
      "dialectEncoding",
    );
  });

  test("ignores unknown values", () => {
    expect(parseParams({ input: "/a/", unknown: 1 }, dev)).not.toHaveProperty(
      "input",
    );
  });
});

describe("paramsOf", () => {
  test("follows the schema with the Playground options", () => {
    expect(params.slice(0, 4).map(({ name }) => name)).toEqual([
      "checker",
      "logger",
      "timeout",
      "dialect",
    ]);
    const names = params.map(({ name }) => name);
    expect(names.slice(4, 7)).toEqual([
      "dialectEncoding",
      "dialectSource",
      "dialectUtf8",
    ]);
    expect(params.find(({ name }) => name === "attackTimeout")?.type).toBe(
      "duration",
    );
    expect(params.find(({ name }) => name === "heatRatio")?.type).toBe("float");
  });

  test("hides the parameters newer than the release", () => {
    const old: Build = { type: "release", version: "4.6.2" };
    const names = paramsOf(old).map(({ name }) => name);
    expect(names).toContain("checker");
    expect(names).not.toContain("dialect");
    expect(parseParams({ dialect: "python" }, old)).not.toHaveProperty(
      "dialect",
    );
    expect(paramsOf({ type: "release", version: "5.0.0" }, "perl")).toEqual(
      params,
    );
    expect(paramsOf(dev, "perl")).toEqual(params);
  });

  test("hides the parameters of the other dialects", () => {
    const names = (dialect?: string) =>
      paramsOf(dev, dialect).map(({ name }) => name);
    expect(names()).not.toContain("dialectEncoding");
    expect(names()).not.toContain("dialectSource");
    expect(names("python")).toContain("dialectEncoding");
    expect(names("python")).not.toContain("dialectUtf8");
    expect(names("perl")).toEqual(
      expect.arrayContaining([
        "dialectEncoding",
        "dialectSource",
        "dialectUtf8",
      ]),
    );
  });
});

describe("selectValues", () => {
  test("gives the choices for the dialect", () => {
    const param = (name: string) => params.find((p) => p.name === name)!;
    expect(selectValues(param("dialect"), "perl")).toEqual([
      "javascript",
      "java",
      "dotnet",
      "pcre",
      "python",
      "ruby",
      "perl",
    ]);
    expect(selectValues(param("dialectEncoding"), "ruby")).toEqual([
      "UTF-8",
      "ASCII-8BIT",
    ]);
    expect(selectValues(param("dialectEncoding"), "javascript")).toEqual([]);
    expect(selectValues(param("dialectSource"), "perl")).toEqual([
      "literal",
      "string",
    ]);
    expect(selectValues(param("dialectUtf8"), "perl")).toEqual([
      "null",
      "true",
      "false",
    ]);
    expect(selectValues(param("checker"), "perl")).toEqual([
      "auto",
      "fuzz",
      "automaton",
    ]);
  });
});

describe("formValues", () => {
  test("converts the parameters back to the form values", () => {
    expect(
      formValues(parseParams({ dialect: "perl", timeout: "3000" }, dev)),
    ).toMatchObject({
      dialect: "perl",
      timeout: 3000,
      dialectEncoding: "characters",
      dialectSource: "literal",
      dialectUtf8: "null",
    });
    expect(formValues({ dialect: "python" })).toEqual({ dialect: "python" });
  });
});

describe("extract", () => {
  test("splits the source and the flags", () => {
    expect(extract("/a/")).toEqual({ source: "a", flags: "" });
    expect(extract("/a/gu")).toEqual({ source: "a", flags: "gu" });
    expect(extract("/a\\/b/")).toEqual({ source: "a\\/b", flags: "" });
    expect(extract("/[/]/u")).toEqual({ source: "[/]", flags: "u" });
  });

  test("rejects inputs that are not a regex literal", () => {
    expect(extract("")).toBeNull();
    expect(extract("a")).toBeNull();
    expect(extract("/")).toBeNull();
    expect(extract("/a")).toBeNull();
  });
});
