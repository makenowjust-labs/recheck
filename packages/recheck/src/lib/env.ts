export type Backend = "auto" | "java" | "native" | "worker" | "pure";
export type SyncBackend = "synckit" | "pure";

/** Returns `RECHECK_BACKEND` environment variable value, or `'auto'` as the default. */
export const RECHECK_BACKEND: () => Backend = () =>
  (process.env["RECHECK_BACKEND"] as Backend) || "auto";

/** Returns `RECHECK_SYNC_BACKEND` environment variable value, or `'synckit'` as the default. */
export const RECHECK_SYNC_BACKEND: () => SyncBackend = () =>
  (process.env["RECHECK_SYNC_BACKEND"] as SyncBackend) || "synckit";

/** Returns `RECHECK_JAR` environment variable value, or `null` as the default. */
export const RECHECK_JAR: () => string | null = () =>
  process.env["RECHECK_JAR"] || null;

/** Returns `RECHECK_BIN` environment variable value, or `null` as the default. */
export const RECHECK_BIN: () => string | null = () =>
  process.env["RECHECK_BIN"] || null;
