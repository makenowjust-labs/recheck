import { extract, params, parseParams } from "./params";

describe("parseParams", () => {
  test("falls back to the default values", () => {
    const parsed = parseParams({});
    expect(parsed.checker).toBe("auto");
    expect(parsed.logger).toBe("off");
    expect(parsed.timeout).toBe(10000);
    expect(parsed.heatRatio).toBe(0.001);
    expect(parsed.recallTimeout).toBe(-1000);
    expect(Object.keys(parsed).sort()).toEqual(
      params.map(({ name }) => name).sort(),
    );
  });

  test("parses the form values as numbers by type", () => {
    const parsed = parseParams({
      checker: "fuzz",
      logger: "on",
      timeout: "3000",
      heatRatio: "0.5",
      maxDegree: "2",
    });
    expect(parsed.checker).toBe("fuzz");
    expect(parsed.logger).toBe("on");
    expect(parsed.timeout).toBe(3000);
    expect(parsed.heatRatio).toBe(0.5);
    expect(parsed.maxDegree).toBe(2);
  });

  test("ignores unknown values", () => {
    expect(parseParams({ input: "/a/", unknown: 1 })).not.toHaveProperty(
      "input",
    );
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
