// @ts-check

module.exports =
  /** @type {import('@jest/types').Config} */
  ({
    roots: ["<rootDir>/src"],
    testMatch: ["**/?(*.)test.+(js|ts|tsx)"],
    transform: {
      "\\.(js|ts)$": ["esbuild-jest", { sourcemap: true }],
    },
    coverageProvider: "v8",
    verbose: true
  });
