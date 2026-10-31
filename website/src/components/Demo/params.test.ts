import { extract, params, paramsOf, parseParams } from "./params";

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
      params.map(({ name }) => name).sort(),
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
    expect(names).not.toContain("encoding");
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
    expect(paramsOf({ type: "release", version: "5.0.0" })).toEqual(params);
    expect(paramsOf(dev)).toEqual(params);
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
