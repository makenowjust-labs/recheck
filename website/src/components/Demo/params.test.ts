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
    expect(parsed.dialect).toBe("javascript");
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

  test("parses the parameters of the dialect", () => {
    expect(parseParams({}, dev)).not.toHaveProperty("encoding");
    expect(parseParams({ perlUtf8: "true" }, dev)).not.toHaveProperty(
      "perlUtf8",
    );
    const perl = parseParams(
      { dialect: "perl", encoding: "bytes", perlUtf8: "true" },
      dev,
    );
    expect(perl).toMatchObject({
      dialect: "perl",
      encoding: "bytes",
      perlSource: "literal",
      perlUtf8: true,
    });
    expect(
      parseParams({ dialect: "perl", perlUtf8: "false" }, dev).perlUtf8,
    ).toBe(false);
    expect(parseParams({ dialect: "perl" }, dev).perlUtf8).toBeNull();
    // An encoding of another dialect falls back to the default.
    expect(
      parseParams({ dialect: "ruby", encoding: "str" }, dev),
    ).toMatchObject({
      encoding: "UTF-8",
    });
    expect(parseParams({ dialect: "python" }, dev)).not.toHaveProperty(
      "perlSource",
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
    expect(names).toContain("perlSource");
    expect(names).toContain("encoding");
    expect(names).toContain("perlUtf8");
    expect(names).not.toContain("recallCommand");
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
    expect(names()).not.toContain("encoding");
    expect(names()).not.toContain("perlSource");
    expect(names("python")).toContain("encoding");
    expect(names("python")).not.toContain("perlUtf8");
    expect(names("perl")).toEqual(
      expect.arrayContaining(["encoding", "perlSource", "perlUtf8"]),
    );
  });
});

describe("selectValues", () => {
  test("gives the choices for the dialect", () => {
    const param = (name: string) => params.find((p) => p.name === name)!;
    expect(selectValues(param("encoding"), "ruby")).toEqual([
      "UTF-8",
      "ASCII-8BIT",
    ]);
    expect(selectValues(param("encoding"), "javascript")).toEqual([]);
    expect(selectValues(param("perlUtf8"), "perl")).toEqual([
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
    ).toMatchObject({ dialect: "perl", timeout: 3000, perlUtf8: "null" });
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
