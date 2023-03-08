// @ts-check

module.exports =
  /** @type {import('jest').Config} */
  ({
    roots: ["<rootDir>/src"],
    testMatch: ["**/?(*.)test.+(js|ts)"],
    moduleNameMapper: {
      "./worker$": "<rootDir>/src/lib/__test__/test-worker.js",
    },
    transform: {
      "\\.(?:js|ts)$": ["esbuild-jest", { sourcemap: true }],
    },
    coverageProvider: "v8",
    coveragePathIgnorePatterns: ["/node_modules/", "/__test__/"],
    verbose: true,
  });
