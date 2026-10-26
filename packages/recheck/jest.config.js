// @ts-check

// Jest strips the types itself when no transformer claims a `.ts` file, so the tests run as ES modules (`--experimental-vm-modules`).
module.exports =
  /** @type {import("jest").Config} */
  ({
    roots: ["<rootDir>/src"],
    testMatch: ["**/?(*.)test.+(js|ts)"],
    testPathIgnorePatterns: ["/node_modules/", "e2e\\.test\\.(?:js|ts)$"],
    moduleNameMapper: {
      "./worker$": "<rootDir>/src/lib/__test__/test-worker.js",
    },
    transform: {},
    extensionsToTreatAsEsm: [".ts"],
    coverageProvider: "v8",
    coveragePathIgnorePatterns: ["/node_modules/", "/__test__/"],
    verbose: true,
  });
