// @ts-check

module.exports =
  /** @type {import("jest").Config} */
  ({
    roots: ["<rootDir>/src"],
    testMatch: ["**/?(*.)test.+(js|ts)"],
    transform: {
      "\\.(?:js|ts)$": ["esbuild-jest", { sourcemap: true }],
    },
    coverageProvider: "v8",
    verbose: true,
  });
