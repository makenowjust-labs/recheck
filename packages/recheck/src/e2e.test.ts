// End-to-end tests against a real backend.
//
// Unlike the other tests, this file tests the built package through both entry points (`index.js` and `index.mjs`) without mocking.
// The backend is specified by the `RECHECK_BACKEND` environment variable.
// It is run by `pnpm run test:e2e` on CI for each backend (`pure`, `worker`, `java` and `native`).
//
// For `java` and `native` backends, the `RECHECK_JAR` or `RECHECK_BIN` environment variable should point to the corresponding artifact.

import * as cjs from "../index.js";
import * as esm from "../index.mjs";
import * as env from "./lib/env";

import type { Diagnostics } from "..";

const entries = [
  ["index.js", cjs],
  ["index.mjs", esm],
] as const;

const backends: env.Backend[] = ["pure", "worker", "java", "native"];
const backend = env.RECHECK_BACKEND();
if (!backends.includes(backend)) {
  throw new Error(
    `RECHECK_BACKEND must be one of ${backends.join(", ")} (got: ${backend})`,
  );
}

type Case = {
  source: string;
  flags: string;
  status: Diagnostics["status"];
  summary?: string;
};

const cases: Case[] = [
  { source: "a", flags: "", status: "safe" },
  {
    source: "^(a|a)+$",
    flags: "",
    status: "vulnerable",
    summary: "exponential",
  },
  {
    source: "^a*a*$",
    flags: "",
    status: "vulnerable",
    summary: "2nd degree polynomial",
  },
  // Non-ASCII characters must be passed to the backend as-is (#1910).
  {
    source: "“[^”]*”",
    flags: "u",
    status: "vulnerable",
    summary: "2nd degree polynomial",
  },
  // Astral characters (surrogate pairs) too.
  {
    source: "😀[^😁]*😁",
    flags: "u",
    status: "vulnerable",
    summary: "2nd degree polynomial",
  },
];

const expectDiagnostics = (diagnostics: Diagnostics, expected: Case): void => {
  expect(diagnostics.source).toBe(expected.source);
  expect(diagnostics.flags).toBe(expected.flags);
  expect(diagnostics.status).toBe(expected.status);
  if (diagnostics.status === "vulnerable") {
    expect(diagnostics.complexity.summary).toBe(expected.summary);
  }
};

describe.each(entries)(
  `backend: ${backend}, entry: %s`,
  (_, { check, checkSync }) => {
    for (const expected of cases) {
      const title = `/${expected.source}/${expected.flags}`;

      test(`check: ${title}`, async () => {
        const diagnostics = await check(expected.source, expected.flags, {
          checker: "automaton",
        });
        expectDiagnostics(diagnostics, expected);
      });

      test(`checkSync: ${title}`, () => {
        const diagnostics = checkSync(expected.source, expected.flags, {
          checker: "automaton",
        });
        expectDiagnostics(diagnostics, expected);
      });
    }
  },
);
