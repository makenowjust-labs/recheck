import { compressToEncodedURIComponent } from "lz-string";

import { parseParams } from "./params";
import {
  decode,
  encode,
  makeSnapshot,
  sameRequest,
  summarize,
} from "./permalink";

import type { Diagnostics } from "recheck";
import type { Build } from "./builds";
import type { Snapshot } from "./permalink";

const build: Build = { type: "release", version: "4.6.1" };
const params = parseParams({});
const date = "2026-10-25T00:00:00.000Z";

const safe: Snapshot = {
  v: 1,
  build,
  input: "/a/",
  params,
  result: { status: "safe", source: "a", flags: "", checker: "automaton" },
  time: 12,
  date,
};

const vulnerable: Snapshot = {
  ...safe,
  input: "/^(a|a)*$/",
  result: {
    status: "vulnerable",
    source: "^(a|a)*$",
    flags: "",
    checker: "automaton",
    complexity: { type: "exponential", summary: "exponential" },
    attackPattern: "'a'.repeat(1000) + '\\x00'",
    hotspot: [{ start: 2, end: 5, temperature: "heat" }],
  },
};

describe("makeSnapshot", () => {
  const snapshot = (
    input: string,
    result: Diagnostics | { status: "invalid" },
  ) => makeSnapshot({ build, input, params, result, time: 12, date });

  test("records only what the result card shows", () => {
    const diagnostics: Diagnostics = {
      source: "^(a|a)*$",
      flags: "",
      status: "vulnerable",
      checker: "automaton",
      complexity: {
        type: "exponential",
        summary: "exponential",
        isFuzz: false,
      },
      attack: {
        pumps: [{ prefix: "", pump: "a", bias: 0 }],
        suffix: "\x00",
        base: 1000,
        string: "a".repeat(1000) + "\x00",
        pattern: "'a'.repeat(1000) + '\\x00'",
      },
      hotspot: [{ start: 2, end: 5, temperature: "heat" }],
    };
    expect(snapshot("/^(a|a)*$/", diagnostics)).toEqual(vulnerable);
  });

  test("keeps the source, the flags and the checker of a safe result", () => {
    const diagnostics: Diagnostics = {
      source: "a",
      flags: "",
      status: "safe",
      checker: "automaton",
      complexity: { type: "safe", summary: "safe", isFuzz: false },
    };
    expect(snapshot("/a/", diagnostics)).toEqual(safe);
  });

  test("keeps the input of an invalid request", () => {
    expect(snapshot("a", { status: "invalid" }).result).toEqual({
      status: "invalid",
      input: "a",
    });
  });

  test("keeps the error message only when there is one", () => {
    const unknown: Diagnostics = {
      source: "a",
      flags: "",
      status: "unknown",
      error: { kind: "timeout" },
    };
    expect(snapshot("/a/", unknown).result).toEqual({
      status: "unknown",
      source: "a",
      flags: "",
      error: { kind: "timeout" },
    });
    expect(
      snapshot("/a/", {
        ...unknown,
        checker: "fuzz",
        error: { kind: "unsupported", message: "lookbehind" },
      }).result,
    ).toEqual({
      status: "unknown",
      source: "a",
      flags: "",
      checker: "fuzz",
      error: { kind: "unsupported", message: "lookbehind" },
    });
  });
});

describe("encode and decode", () => {
  test("round-trips a snapshot through the hash", () => {
    for (const snapshot of [safe, vulnerable]) {
      const hash = encode(snapshot);
      expect(hash).toMatch(/^r=[A-Za-z0-9+\-$]+$/);
      expect(decode(hash)).toEqual(snapshot);
      expect(decode(`#${hash}`)).toEqual(snapshot);
    }
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
    expect(sameRequest(safe, safe.input, params)).toBe(true);
    expect(sameRequest(safe, "/b/", params)).toBe(false);
    expect(
      sameRequest(safe, safe.input, parseParams({ checker: "fuzz" })),
    ).toBe(false);
  });
});

describe("summarize", () => {
  test("includes the complexity only for vulnerable results", () => {
    expect(summarize(safe.result)).toBe("safe");
    expect(summarize(vulnerable.result)).toBe("vulnerable (exponential)");
    expect(summarize({ status: "invalid", input: "a" })).toBe("invalid");
  });
});
