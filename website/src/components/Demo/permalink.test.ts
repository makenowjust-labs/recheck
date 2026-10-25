import { compressToEncodedURIComponent } from "lz-string";

import { parseParams } from "./params";
import { decode, encode, sameRequest, summarize } from "./permalink";

import type { Build } from "./builds";
import type { Snapshot } from "./permalink";

const build: Build = { type: "release", version: "4.6.1" };

const safe: Snapshot = {
  v: 1,
  build,
  input: "/a/",
  params: parseParams({}),
  result: {
    source: "a",
    flags: "",
    status: "safe",
    checker: "automaton",
    complexity: { type: "safe", summary: "safe", isFuzz: false },
  },
  time: 12,
  date: "2026-10-25T00:00:00.000Z",
};

const vulnerable: Snapshot = {
  ...safe,
  input: "/^(a|a)*$/",
  result: {
    source: "^(a|a)*$",
    flags: "",
    status: "vulnerable",
    checker: "automaton",
    complexity: { type: "exponential", summary: "exponential", isFuzz: false },
    attack: {
      pumps: [{ prefix: "", pump: "a", bias: 0 }],
      suffix: "\x00",
      base: 1000,
      string: "a".repeat(1000) + "\x00",
      pattern: "'a'.repeat(1000) + '\\x00'",
    },
    hotspot: [],
  },
};

describe("encode and decode", () => {
  test("round-trips a snapshot through the hash", () => {
    const hash = encode(safe);
    expect(hash).toMatch(/^r=[A-Za-z0-9+\-$]+$/);
    expect(decode(hash)).toEqual(safe);
    expect(decode(`#${hash}`)).toEqual(safe);
  });

  test("drops the attack string but keeps the attack pattern", () => {
    const decoded = decode(encode(vulnerable));
    expect(decoded?.result).toMatchObject({
      status: "vulnerable",
      attack: { pattern: "'a'.repeat(1000) + '\\x00'", string: "" },
    });
    expect(encode(vulnerable).length).toBeLessThan(
      JSON.stringify(vulnerable).length,
    );
  });

  test("returns null for anything that is not a snapshot", () => {
    expect(decode("")).toBeNull();
    expect(decode("#")).toBeNull();
    expect(decode("#foo")).toBeNull();
    expect(decode("#r=")).toBeNull();
    expect(decode("#r=not-a-compressed-string")).toBeNull();
    expect(
      decode(`#r=${compressToEncodedURIComponent(JSON.stringify({ v: 2 }))}`),
    ).toBeNull();
  });
});

describe("sameRequest", () => {
  test("requires the same input and parameters", () => {
    expect(sameRequest(safe, safe.input, parseParams({}))).toBe(true);
    expect(sameRequest(safe, "/b/", parseParams({}))).toBe(false);
    expect(
      sameRequest(safe, safe.input, parseParams({ checker: "fuzz" })),
    ).toBe(false);
  });
});

describe("summarize", () => {
  test("includes the complexity only for vulnerable results", () => {
    expect(summarize(safe.result)).toBe("safe");
    expect(summarize(vulnerable.result)).toBe("vulnerable (exponential)");
    expect(summarize({ input: "a", status: "invalid" })).toBe("invalid");
  });
});
