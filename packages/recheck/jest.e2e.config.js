// @ts-check

// Jest configuration for the end-to-end tests against a real backend (`src/e2e.test.ts`).

module.exports =
  /** @type {import("jest").Config} */
  ({
    roots: ["<rootDir>/src"],
    testMatch: ["**/?(*.)e2e.test.+(js|ts)"],
    transform: {
      "\\.(?:js|ts)$": ["esbuild-jest", { sourcemap: true }],
    },
    verbose: true,
  });
