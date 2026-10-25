// @ts-check

// Jest configuration for the end-to-end tests against a real backend (`src/e2e.test.ts`).
// The tests run as ES modules, so Jest needs `--experimental-vm-modules` (see `test:e2e` in `package.json`).

module.exports =
  /** @type {import("jest").Config} */
  ({
    roots: ["<rootDir>/src"],
    testMatch: ["**/?(*.)e2e.test.+(js|ts)"],
    transform: {},
    extensionsToTreatAsEsm: [".ts"],
    testTimeout: 60 * 1000,
    verbose: true,
  });
