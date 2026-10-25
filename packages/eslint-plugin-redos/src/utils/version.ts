import { nodeRequire } from "./require";

/** Returns the version string of `recheck` package. */
export const recheckVersion = (): string => {
  const pkg = nodeRequire("recheck/package.json") as unknown as {
    version: string;
  };
  return pkg.version;
};
