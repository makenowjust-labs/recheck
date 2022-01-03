export type Backend = "auto" | "java" | "native" | "pure";

/** Returns `RECHECK_BACKEND` environment variable value, or `'auto'` as the default. */
export const RECHECK_BACKEND: () => Backend = () =>
  (process.env["RECHECK_BACKEND"] as Backend) || "auto";

/** Returns `RECHECK_JAR` environment variable value, or `null` as the default. */
export const RECHECK_JAR: () => string | null = () =>
  process.env["RECHECK_JAR"] || null;

/** Returns `RECHECK_BIN` environment variable value, or `null` as the default. */
export const RECHECK_BIN: () => string | null = () =>
  process.env["RECHECK_BIN"] || null;
