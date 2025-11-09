import * as env from "./env";

/** Exposes this to mock `require.resolve` on testing. */
export const __mock__require = require;

/** Returns `recheck.jar` file path, or `null` if it is not found. */
export const jar: () => string | null = () => {
  const jarEnv = env.RECHECK_JAR();
  if (jarEnv !== null) {
    return jarEnv;
  }

  try {
    const exe = __mock__require
      .resolve("recheck-jar/package.json")
      .replace(/package\.json$/, "recheck.jar");
    return exe;
  } catch (err: any) {
    if (err && err.code == "MODULE_NOT_FOUND") {
      return null;
    }

    throw err;
  }
};

/** A mapping from a supported platform (OS) name to the corresponding package name component. */
export const osNames: Record<string, string> = {
  darwin: "macos",
  linux: "linux",
  win32: "windows",
};

/** A mapping from a supported architecture (CPU) name to the corresponding package name component. */
export const cpuNames: Record<string, string> = {
  x64: "x64",
  arm64: "arm64",
};

/** Returns `recheck` (or `recheck.exe`) binary file path, or `null` if it is not found. */
export const bin: () => string | null = () => {
  const binEnv = env.RECHECK_BIN();
  if (binEnv !== null) {
    return binEnv;
  }

  const os = osNames[process.platform];
  const cpu = cpuNames[process.arch];
  const isWin32 = os === "windows";

  // When `os` or `cpu` is not available, it means this platform is not supported.
  /* c8 ignore next 3 */
  if (!os || !cpu) {
    return null;
  }

  try {
    // Constructs a package name with a binary file name, and resolves this.
    // If it is succeeded, we expect that the result path is `recheck` CLI.
    /* c8 ignore next */
    const bin = isWin32 ? "recheck.exe" : "recheck";
    const pkg = `recheck-${os}-${cpu}/package.json`;
    const exe = __mock__require.resolve(pkg).replace(/package\.json$/, bin);
    return exe;
  } catch (err: any) {
    if (err && err.code == "MODULE_NOT_FOUND") {
      return null;
    }

    throw err;
  }
};
