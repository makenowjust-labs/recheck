import {
  compressToEncodedURIComponent,
  decompressFromEncodedURIComponent,
} from "lz-string";

import type { Diagnostics } from "recheck";
import type { Build } from "./builds";
import type { CheckParams } from "./params";

export type InvalidDiagnostics = { input: string; status: "invalid" };

/** A result and everything needed to reproduce it, as stored in a permalink. */
export type Snapshot = {
  v: 1;
  build: Build;
  input: string;
  params: CheckParams;
  result: Diagnostics | InvalidDiagnostics;
  time: number;
  date: string;
};

const PREFIX = "r=";

/** Whether the snapshot was recorded for the given input and parameters. */
export const sameRequest = (
  snapshot: Snapshot,
  input: string,
  params: CheckParams,
): boolean =>
  snapshot.input === input &&
  JSON.stringify(snapshot.params) === JSON.stringify(params);

export const summarize = (result: Snapshot["result"]): string =>
  result.status === "vulnerable"
    ? `${result.status} (${result.complexity.summary})`
    : result.status;

// The expanded attack string can be hundreds of kilobytes, and it is derivable from the other fields.
const stripAttackString = (result: Snapshot["result"]): Snapshot["result"] =>
  result.status === "vulnerable"
    ? { ...result, attack: { ...result.attack, string: "" } }
    : result;

export const encode = (snapshot: Snapshot): string =>
  PREFIX +
  compressToEncodedURIComponent(
    JSON.stringify({ ...snapshot, result: stripAttackString(snapshot.result) }),
  );

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
