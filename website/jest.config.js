// @ts-check

// Jest strips the types itself when no transformer claims a `.ts` file, so the tests run as ES modules (`--experimental-vm-modules`).
module.exports =
  /** @type {import("jest").Config} */
  ({
    roots: ["<rootDir>/src"],
    testMatch: ["**/?(*.)test.+(js|ts)"],
    transform: {},
    extensionsToTreatAsEsm: [".ts"],
    coverageProvider: "v8",
    verbose: true,
  });
