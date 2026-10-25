import {
  compressToEncodedURIComponent,
  decompressFromEncodedURIComponent,
} from "lz-string";

import type {
  AttackPattern,
  Diagnostics,
  VulnerableDiagnostics,
} from "recheck";
import type { Build } from "./builds";
import type { CheckParams } from "./params";

export type InvalidDiagnostics = { input: string; status: "invalid" };

// The expanded attack string can be hundreds of kilobytes, and it is derivable from the other fields.
export type Result =
  | Exclude<Diagnostics, VulnerableDiagnostics>
  | (Omit<VulnerableDiagnostics, "attack"> & {
      attack: Omit<AttackPattern, "string">;
    })
  | InvalidDiagnostics;

/** A result and everything needed to reproduce it, as stored in a permalink. */
export type Snapshot = {
  v: 1;
  build: Build;
  input: string;
  params: CheckParams;
  result: Result;
  time: number;
  date: string;
};

const PREFIX = "r=";

export const toResult = (diagnostics: Diagnostics): Result => {
  if (diagnostics.status !== "vulnerable") return diagnostics;
  const { string: _string, ...attack } = diagnostics.attack;
  return { ...diagnostics, attack };
};

/** Whether the snapshot was recorded for the given input and parameters. */
export const sameRequest = (
  snapshot: Snapshot,
  input: string,
  params: CheckParams,
): boolean =>
  snapshot.input === input &&
  JSON.stringify(snapshot.params) === JSON.stringify(params);

export const summarize = (result: Result): string =>
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
