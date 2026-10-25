import { compressToEncodedURIComponent, decompressFromEncodedURIComponent } from 'lz-string';

import type { Diagnostics } from 'recheck';
import type { CheckParams } from './params';

/** Identifies the `recheck` build that produced a result. */
export type Build = { type: 'release'; version: string } | { type: 'head'; commit: string } | { type: 'dev' };

export type InvalidDiagnostics = { input: string; status: 'invalid' };

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

const PREFIX = 'r=';

export const formatBuild = (build: Build): string => {
  switch (build.type) {
    case 'release':
      return `v${build.version}`;
    case 'head':
      return `HEAD (${build.commit.slice(0, 7)})`;
    case 'dev':
      return 'development build';
  }
};

export const sameBuild = (a: Build, b: Build): boolean => formatBuild(a) === formatBuild(b);

/** The expanded attack string can be hundreds of kilobytes, and it is derivable from the other fields. */
const stripAttackString = (result: Snapshot['result']): Snapshot['result'] =>
  result.status === 'vulnerable' ? { ...result, attack: { ...result.attack, string: '' } } : result;

export const encode = (snapshot: Snapshot): string =>
  PREFIX + compressToEncodedURIComponent(JSON.stringify({ ...snapshot, result: stripAttackString(snapshot.result) }));

export const decode = (hash: string): Snapshot | null => {
  const body = hash.replace(/^#/, '');
  if (!body.startsWith(PREFIX)) return null;
  try {
    const snapshot = JSON.parse(decompressFromEncodedURIComponent(body.slice(PREFIX.length)) ?? '');
    return snapshot?.v === 1 ? (snapshot as Snapshot) : null;
  } catch {
    return null;
  }
};
