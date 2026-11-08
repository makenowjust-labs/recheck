import { compressToEncodedURIComponent } from "lz-string";

import { parseParams } from "./params";
import {
  decode,
  encode,
  hotspotOf,
  makeRepairSnapshot,
  makeSnapshot,
  sameRequest,
  summarize,
} from "./permalink";

import type { Diagnostics, Rx } from "recheck";
import type { Build } from "./builds";
import type { Snapshot } from "./permalink";

const build: Build = { type: "release", version: "4.6.1" };
const params = parseParams({}, build);
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
    hotspot: [
      { start: 2, end: 3, temperature: "heat" },
      { start: 4, end: 5, temperature: "normal" },
    ],
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
      dialect: { name: "javascript" },
      unitMode: "utf16",
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
      hotspot: [
        { start: 2, end: 3, temperature: "hot" },
        { start: 4, end: 5, temperature: "warm" },
      ],
      runtimeAttack: null,
      upperBound: null,
      refutedAttacks: [],
    };
    expect(snapshot("/^(a|a)*$/", diagnostics)).toEqual(vulnerable);
  });

  test("keeps the upper bound, with the temperatures of the permalinks", () => {
    const diagnostics: Diagnostics = {
      source: "^(a|aa)\\1*$",
      flags: "",
      dialect: { name: "javascript" },
      unitMode: "utf16",
      status: "safe",
      checker: "fuzz",
      complexity: { type: "safe", summary: "safe (fuzz)", isFuzz: true },
      upperBound: {
        complexity: {
          type: "exponential",
          summary: "exponential",
          isFuzz: false,
        },
        hotspot: [{ start: 2, end: 3, temperature: "hot" }],
      },
      refutedAttacks: [],
    };
    expect(snapshot("/^(a|aa)\\1*$/", diagnostics).result).toEqual({
      status: "safe",
      source: "^(a|aa)\\1*$",
      flags: "",
      checker: "fuzz",
      upperBound: {
        complexity: { type: "exponential", summary: "exponential" },
        hotspot: [{ start: 2, end: 3, temperature: "heat" }],
      },
    });
  });

  test("records no upper bound for the diagnostics of an older release, which lack it", () => {
    const { upperBound: _, ...older } = {
      source: "a",
      flags: "",
      dialect: { name: "javascript" },
      unitMode: "utf16",
      status: "safe",
      checker: "automaton",
      complexity: { type: "safe", summary: "safe", isFuzz: false },
      upperBound: null,
      refutedAttacks: [],
    } satisfies Diagnostics;
    expect(snapshot("/a/", older as unknown as Diagnostics)).toEqual(safe);
  });

  test("keeps the source, the flags and the checker of a safe result", () => {
    const diagnostics: Diagnostics = {
      source: "a",
      flags: "",
      dialect: { name: "javascript" },
      unitMode: "utf16",
      status: "safe",
      checker: "automaton",
      complexity: { type: "safe", summary: "safe", isFuzz: false },
      upperBound: null,
      refutedAttacks: [],
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
      dialect: { name: "javascript" },
      unitMode: null,
      status: "unknown",
      error: { kind: "timeout" },
      upperBound: null,
      refutedAttacks: [],
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
        upperBound: {
          complexity: { type: "linear", summary: "linear", isFuzz: false },
          hotspot: [],
        },
      }).result,
    ).toEqual({
      status: "unknown",
      source: "a",
      flags: "",
      checker: "fuzz",
      error: { kind: "timeout" },
      upperBound: {
        complexity: { type: "linear", summary: "linear" },
        hotspot: [],
      },
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

describe("makeRepairSnapshot", () => {
  const rx = {
    source: "^(a|a)*$",
    flags: "",
    dialect: { name: "javascript" },
  } as const;

  test("records only what the repair card shows", () => {
    const repaired: Rx = {
      ...rx,
      unitMode: "utf16",
      status: "repaired",
      repaired: "^(a)*$",
      complexity: { type: "linear", summary: "linear", isFuzz: false },
      kept: [0, 1],
      comparison: {
        kind: "different",
        input: "aa",
        original: [
          [0, 2],
          [1, 2],
        ],
        repaired: null,
      },
    };
    expect(makeRepairSnapshot("all", repaired, 34)).toEqual({
      keep: "all",
      result: {
        status: "repaired",
        repaired: "^(a)*$",
        complexity: { type: "linear", summary: "linear" },
        kept: [0, 1],
        comparison: {
          kind: "different",
          input: "aa",
          original: [
            [0, 2],
            [1, 2],
          ],
          repaired: null,
        },
      },
      time: 34,
    });
  });

  test("keeps the error only when there is one", () => {
    const unknown = (error: Extract<Rx, { status: "unknown" }>["error"]) =>
      makeRepairSnapshot("match", { ...rx, status: "unknown", error }, 5)
        .result;
    expect(unknown(null)).toEqual({ status: "unknown", error: null });
    expect(unknown({ kind: "timeout" })).toEqual({
      status: "unknown",
      error: { kind: "timeout" },
    });
    expect(unknown({ kind: "unsupported", message: "recursion" })).toEqual({
      status: "unknown",
      error: { kind: "unsupported", message: "recursion" },
    });
    expect(
      makeRepairSnapshot("named", { ...rx, status: "safe" }, 5).result,
    ).toEqual({ status: "safe" });
  });
});

describe("encode and decode", () => {
  test("round-trips a snapshot through the hash", () => {
    const repaired: Snapshot = {
      ...vulnerable,
      repair: {
        keep: "match-range",
        result: {
          status: "repaired",
          repaired: "^(a)*$",
          complexity: { type: "linear", summary: "linear" },
          kept: [0],
          comparison: { kind: "equivalent" },
        },
        time: 34,
      },
    };
    for (const snapshot of [safe, vulnerable, repaired]) {
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
    expect(sameRequest(safe, safe.input, params, build)).toBe(true);
    expect(sameRequest(safe, "/b/", params, build)).toBe(false);
    expect(
      sameRequest(
        safe,
        safe.input,
        parseParams({ checker: "fuzz" }, build),
        build,
      ),
    ).toBe(false);
  });

  test("counts a parameter of a newer build as recorded with its default", () => {
    const newer: Build = { type: "release", version: "5.0.0-beta.1" };
    expect(sameRequest(safe, safe.input, parseParams({}, newer), newer)).toBe(
      true,
    );
    expect(
      sameRequest(
        safe,
        safe.input,
        parseParams({ dialect: "ruby" }, newer),
        newer,
      ),
    ).toBe(false);
    expect(
      sameRequest(
        safe,
        safe.input,
        parseParams({ logger: "on" }, build),
        build,
      ),
    ).toBe(false);
  });

  test("rejects a parameter that the build does not have", () => {
    const recorded: Snapshot = {
      ...safe,
      params: { ...params, ...parseParams({}, { type: "dev" }) },
    };
    expect(sameRequest(recorded, safe.input, params, build)).toBe(false);
  });
});

describe("hotspotOf", () => {
  test("maps the temperatures of the permalinks to those of Diagnostics", () => {
    const { result } = vulnerable;
    expect(result.status === "vulnerable" && hotspotOf(result)).toEqual([
      { start: 2, end: 3, temperature: "hot" },
      { start: 4, end: 5, temperature: "warm" },
    ]);
    expect(
      hotspotOf({ hotspot: [{ start: 0, end: 1, temperature: "normal" }] }),
    ).toEqual([{ start: 0, end: 1, temperature: "warm" }]);
  });
});

describe("summarize", () => {
  test("includes the complexity only for vulnerable results", () => {
    expect(summarize(safe.result)).toBe("safe");
    expect(summarize(vulnerable.result)).toBe("vulnerable (exponential)");
    expect(summarize({ status: "invalid", input: "a" })).toBe("invalid");
  });
});
