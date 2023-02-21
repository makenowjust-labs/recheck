/** Returns the version string of `recheck` package. */
export const recheckVersion = (): string => {
  const pkg = require.resolve("recheck/package.json") as unknown as {
    version: string;
  };
  return pkg.version;
};
