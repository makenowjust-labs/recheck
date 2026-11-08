import {
  compressToEncodedURIComponent,
  decompressFromEncodedURIComponent,
} from "lz-string";

import { parseParams } from "./params";

import type { Diagnostics, Hotspot, Match, Rx, UpperBound } from "recheck";
import type { Build } from "./builds";
import type { CheckParams } from "./params";

export type InvalidInput = { status: "invalid" };

type Checker = "automaton" | "fuzz";

// The permalinks keep the temperatures of 4.x (`heat` and `normal`), which are now `hot` and `warm`.
type HotspotSnapshot = {
  start: number;
  end: number;
  temperature: "heat" | "normal";
}[];

// The permalinks recorded before the upper bound lack it.
export type UpperBoundSnapshot = {
  complexity: {
    type: "linear" | "polynomial" | "exponential";
    summary: string;
  };
  hotspot: HotspotSnapshot;
};

// Only what the result card shows is recorded.
// It keeps the permalink format apart from changes of `Diagnostics` and leaves out the expanded attack string.
export type DiagnosticsSnapshot =
  | { status: "invalid"; input: string }
  | {
      status: "safe";
      source: string;
      flags: string;
      checker: Checker;
      upperBound?: UpperBoundSnapshot;
    }
  | {
      status: "vulnerable";
      source: string;
      flags: string;
      checker: Checker;
      complexity: { type: "polynomial" | "exponential"; summary: string };
      attackPattern: string;
      hotspot: HotspotSnapshot;
      upperBound?: UpperBoundSnapshot;
    }
  | {
      status: "unknown";
      source: string;
      flags: string;
      checker?: Checker;
      error: { kind: string; message?: string };
      upperBound?: UpperBoundSnapshot;
    };

export type Keep = "all" | "named" | "match-range" | "match";

// Only what the repair card shows is recorded, as for `DiagnosticsSnapshot`.
export type RxSnapshot =
  | { status: "safe" }
  | {
      status: "repaired";
      repaired: string;
      complexity: { type: string; summary: string };
      kept: number[];
      comparison:
        | { kind: "equivalent" }
        | { kind: "unknown" }
        | {
            kind: "different";
            input: string | number[];
            original: Match | null;
            repaired: Match | null;
          };
    }
  | { status: "unknown"; error: { kind: string; message?: string } | null };

/** A repair of the vulnerable result of a snapshot. */
export type RepairSnapshot = {
  keep: Keep;
  result: RxSnapshot;
  time: number;
};

/** A result and everything needed to reproduce it, as stored in a permalink. */
export type Snapshot = {
  v: 1;
  build: Build;
  input: string;
  params: CheckParams;
  result: DiagnosticsSnapshot;
  time: number;
  date: string;
  repair?: RepairSnapshot;
};

const PREFIX = "r=";

const makeSnapshotHotspot = (hotspot: Hotspot[]): HotspotSnapshot =>
  hotspot.map(({ start, end, temperature }) => ({
    start,
    end,
    temperature: temperature === "hot" ? "heat" : "normal",
  }));

// A missing upper bound is left out, so that the permalink stays short.
// The diagnostics of the releases before the upper bound lack the field.
const makeSnapshotUpperBound = (
  upperBound: UpperBound | null | undefined,
): { upperBound?: UpperBoundSnapshot } => {
  if (upperBound === null || upperBound === undefined) return {};
  const { type, summary } = upperBound.complexity;
  return {
    upperBound: {
      complexity: { type, summary },
      hotspot: makeSnapshotHotspot(upperBound.hotspot),
    },
  };
};

const makeSnapshotDiagnostics = (
  input: string,
  result: Diagnostics | InvalidInput,
): DiagnosticsSnapshot => {
  switch (result.status) {
    case "invalid":
      return { status: "invalid", input };
    case "safe":
      return {
        status: "safe",
        source: result.source,
        flags: result.flags,
        checker: result.checker,
        ...makeSnapshotUpperBound(result.upperBound),
      };
    case "vulnerable":
      return {
        status: "vulnerable",
        source: result.source,
        flags: result.flags,
        checker: result.checker,
        complexity: {
          type: result.complexity.type,
          summary: result.complexity.summary,
        },
        attackPattern: result.attack.pattern,
        hotspot: makeSnapshotHotspot(result.hotspot),
        ...makeSnapshotUpperBound(result.upperBound),
      };
    case "unknown":
      return {
        status: "unknown",
        source: result.source,
        flags: result.flags,
        checker: result.checker,
        error:
          "message" in result.error
            ? { kind: result.error.kind, message: result.error.message }
            : { kind: result.error.kind },
        ...makeSnapshotUpperBound(result.upperBound),
      };
  }
};

export const makeSnapshot = ({
  build,
  input,
  params,
  result,
  time,
  date,
}: {
  build: Build;
  input: string;
  params: CheckParams;
  result: Diagnostics | InvalidInput;
  time: number;
  date: string;
}): Snapshot => ({
  v: 1,
  build,
  input,
  params,
  result: makeSnapshotDiagnostics(input, result),
  time,
  date,
});

export const makeRepairSnapshot = (
  keep: Keep,
  rx: Rx,
  time: number,
): RepairSnapshot => {
  switch (rx.status) {
    case "safe":
      return { keep, result: { status: "safe" }, time };
    case "repaired":
      return {
        keep,
        result: {
          status: "repaired",
          repaired: rx.repaired,
          complexity: {
            type: rx.complexity.type,
            summary: rx.complexity.summary,
          },
          kept: rx.kept,
          comparison: rx.comparison,
        },
        time,
      };
    case "unknown": {
      const error =
        rx.error === null
          ? null
          : "message" in rx.error
            ? { kind: rx.error.kind, message: rx.error.message }
            : { kind: rx.error.kind };
      return { keep, result: { status: "unknown", error }, time };
    }
  }
};

/** Whether the snapshot was recorded for the given input and the parameters of the build.
 *
 * A parameter that the snapshot lacks, because its build was older, counts as recorded with the default of the build.
 */
export const sameRequest = (
  snapshot: Snapshot,
  input: string,
  params: CheckParams,
  build: Build,
): boolean => {
  const recorded: Record<string, unknown> = snapshot.params;
  const defaults: Record<string, unknown> = parseParams({}, build);
  return (
    snapshot.input === input &&
    Object.keys(recorded).every((name) => name in params) &&
    Object.entries(params).every(
      ([name, value]) =>
        JSON.stringify(name in recorded ? recorded[name] : defaults[name]) ===
        JSON.stringify(value),
    )
  );
};

export const summarize = (result: DiagnosticsSnapshot): string =>
  result.status === "vulnerable"
    ? `${result.status} (${result.complexity.summary})`
    : result.status;

export const encode = (snapshot: Snapshot): string =>
  PREFIX + compressToEncodedURIComponent(JSON.stringify(snapshot));

export const decode = (hash: string): Snapshot | null => {
  const body = hash.replace(/^#/, "");
  if (!body.startsWith(PREFIX)) return null;
  try {
    const snapshot = JSON.parse(
      decompressFromEncodedURIComponent(body.slice(PREFIX.length)) ?? "",
    );
    return snapshot?.v === 1 ? (snapshot as Snapshot) : null;
  } catch {
    return null;
  }
};

/** Returns the hotspot of the snapshot, or of its upper bound, with the temperatures of `Diagnostics`. */
export const hotspotOf = (snapshot: { hotspot: HotspotSnapshot }): Hotspot[] =>
  snapshot.hotspot.map(({ start, end, temperature }) => ({
    start,
    end,
    temperature: temperature === "heat" ? "hot" : "warm",
  }));
