// @ts-check

// Jest strips the types itself when no transformer claims a `.ts` file, so the tests run as ES modules (`--experimental-vm-modules`).
export default /** @type {import("jest").Config} */ ({
  testMatch: ["<rootDir>/*.test.ts"],
  transform: {},
  extensionsToTreatAsEsm: [".ts"],
  coverageProvider: "v8",
  verbose: true,
});
