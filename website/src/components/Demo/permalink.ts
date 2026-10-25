import {
  compressToEncodedURIComponent,
  decompressFromEncodedURIComponent,
} from "lz-string";

import type { Diagnostics } from "recheck";
import type { Build } from "./builds";
import type { CheckParams } from "./params";

export type InvalidInput = { status: "invalid" };

type Checker = "automaton" | "fuzz";

// Only what the result card shows is recorded.
// It keeps the permalink format apart from changes of `Diagnostics` and leaves out the expanded attack string.
export type DiagnosticsSnapshot =
  | { status: "invalid"; input: string }
  | { status: "safe"; source: string; flags: string; checker: Checker }
  | {
      status: "vulnerable";
      source: string;
      flags: string;
      checker: Checker;
      complexity: { type: "polynomial" | "exponential"; summary: string };
      attackPattern: string;
      hotspot: { start: number; end: number; temperature: "heat" | "normal" }[];
    }
  | {
      status: "unknown";
      source: string;
      flags: string;
      checker?: Checker;
      error: { kind: string; message?: string };
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
};

const PREFIX = "r=";

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
        hotspot: result.hotspot.map(({ start, end, temperature }) => ({
          start,
          end,
          temperature,
        })),
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

/** Whether the snapshot was recorded for the given input and parameters. */
export const sameRequest = (
  snapshot: Snapshot,
  input: string,
  params: CheckParams,
): boolean =>
  snapshot.input === input &&
  JSON.stringify(snapshot.params) === JSON.stringify(params);

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
