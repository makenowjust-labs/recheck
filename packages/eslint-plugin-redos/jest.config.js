// @ts-check

module.exports =
  /** @type {import("jest").Config} */
  ({
    roots: ["<rootDir>/src"],
    testMatch: ["**/?(*.)test.+(js|ts|tsx)"],
    transform: {
      "\\.(js|ts)$": ["esbuild-jest", { sourcemap: true }],
    },
    transformIgnorePatterns: [
      'node_modules/(?!(?:.+/)?(?:find-cache-dir|find-up|locate-path|p-limit|p-locate|path-exists|pkg-dir|yocto-queue)/)',
    ],
    coverageProvider: "v8",
    verbose: true
  });
