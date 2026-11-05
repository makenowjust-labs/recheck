import { fileURLToPath } from "node:url";

import { afterEach, beforeEach, expect, jest, test } from "@jest/globals";

const RECHECK_JAR = fileURLToPath(
  new URL("../../../target/recheck.jar", import.meta.url),
);
const RECHECK_BIN = fileURLToPath(
  new URL("../../../target/native-image/recheck", import.meta.url),
);

jest.setTimeout(30 * 1000);

jest.unstable_mockModule("synckit", () => ({ createSyncFn: jest.fn() }));
jest.unstable_mockModule("./lib/env", () => ({
  RECHECK_BACKEND: jest.fn(),
  RECHECK_SYNC_BACKEND: jest.fn(),
  RECHECK_BIN: jest.fn(),
  RECHECK_JAR: jest.fn(),
}));
jest.unstable_mockModule("./lib/java", () => ({ ensure: jest.fn() }));
jest.unstable_mockModule("./lib/native", () => ({ ensure: jest.fn() }));

const synckit = await import("synckit");
const agent = await import("./lib/agent");
const env = await import("./lib/env");
const java = await import("./lib/java");
const native = await import("./lib/native");
const main = await import("./main");

const backend = jest.mocked(env.RECHECK_BACKEND);
const syncBackend = jest.mocked(env.RECHECK_SYNC_BACKEND);
const javaEnsure = jest.mocked(java.ensure);
const nativeEnsure = jest.mocked(native.ensure);

beforeEach(() => {
  main.__mock__.agent = undefined;
  main.__mock__.pool = null;
});

afterEach(() => {
  main.__mock__.agent?.kill();
  main.__mock__.agent = undefined;
  main.__mock__.pool?.kill();
  main.__mock__.pool = null;
});

test("check: auto (java)", async () => {
  backend.mockReturnValueOnce("auto");
  javaEnsure.mockImplementationOnce(() =>
    agent.start("java", ["-jar", RECHECK_JAR, "agent"]),
  );

  const diagnostics = await main.check("^(a|a)+$", "");
  expect(diagnostics.status).toBe("vulnerable");
  expect(javaEnsure).toHaveBeenCalled();
});

test("check: auto (fallback)", async () => {
  backend.mockReturnValueOnce("auto");
  javaEnsure.mockResolvedValueOnce(null);
  nativeEnsure.mockResolvedValueOnce(null);

  const diagnostics = await main.check("^(a|a)+$", "");
  expect(diagnostics.status).toBe("vulnerable");
  expect(javaEnsure).toHaveBeenCalled();
  expect(nativeEnsure).toHaveBeenCalled();
});

test("check: auto (fallback, error)", async () => {
  backend.mockReturnValueOnce("auto");
  javaEnsure.mockRejectedValueOnce(new Error("java.ensure error"));
  nativeEnsure.mockRejectedValueOnce(new Error("native.ensure error"));

  const diagnostics = await main.check("^(a|a)+$", "");
  expect(diagnostics.status).toBe("vulnerable");
  expect(javaEnsure).toHaveBeenCalled();
  expect(nativeEnsure).toHaveBeenCalled();
});

test("check: java (1)", async () => {
  backend.mockReturnValueOnce("java");
  javaEnsure.mockImplementationOnce(() =>
    agent.start("java", ["-jar", RECHECK_JAR, "agent"]),
  );

  const diagnostics = await main.check("^(a|a)+$", "");
  expect(diagnostics.status).toBe("vulnerable");
  expect(javaEnsure).toHaveBeenCalled();
});

test("check: java (2)", async () => {
  backend.mockReturnValueOnce("java");
  javaEnsure.mockResolvedValueOnce(null);

  await expect(main.check("^(a|a)+$", "")).rejects.toThrow(
    "there is no available implementation",
  );
});

// This test is skipped because the native binary is not available on CI environment.
test.skip("check: native (1)", async () => {
  backend.mockReturnValueOnce("native");
  nativeEnsure.mockImplementationOnce(() =>
    agent.start(RECHECK_BIN, ["agent"]),
  );

  const diagnostics = await main.check("^(a|a)+$", "");
  expect(diagnostics.status).toBe("vulnerable");
  expect(nativeEnsure).toHaveBeenCalled();
});

test("check: native (2)", async () => {
  backend.mockReturnValueOnce("native");
  nativeEnsure.mockResolvedValueOnce(null);

  await expect(main.check("^(a|a)+$", "")).rejects.toThrow(
    "there is no available implementation",
  );
});

test("check: worker", async () => {
  backend.mockReturnValueOnce("worker");

  const diagnostics = await main.check("^(a|a)+$", "");
  expect(diagnostics.status).toBe("vulnerable");
});

test("check: pure", async () => {
  backend.mockReturnValueOnce("pure");

  const diagnostics = await main.check("^(a|a)+$", "");
  expect(diagnostics.status).toBe("vulnerable");
});

test("check: invalid", async () => {
  backend.mockReturnValueOnce("invalid" as any);

  await expect(main.check("^(a|a)+$", "")).rejects.toThrow(
    "invalid backend: invalid",
  );
});

// The synckit function is created once per module, so the synckit tests share this one.
const synckitFn = jest.fn((method: unknown, ..._: unknown[]) =>
  method === "check" ? { status: "vulnerable" } : { status: "safe" },
);

test("checkSync: synckit", () => {
  syncBackend.mockReturnValueOnce("synckit");
  jest.mocked(synckit.createSyncFn).mockReturnValue(synckitFn as any);

  const diagnostics = main.checkSync("^(a|a)+$", "");
  expect(diagnostics.status).toBe("vulnerable");
  expect(synckitFn).toHaveBeenCalledWith("check", "^(a|a)+$", "", {});
});

test("repairSync: synckit", () => {
  syncBackend.mockReturnValueOnce("synckit");
  jest.mocked(synckit.createSyncFn).mockReturnValue(synckitFn as any);

  const rx = main.repairSync("^(a+)+$", "", { keep: "match" });
  expect(rx.status).toBe("safe");
  expect(synckitFn).toHaveBeenCalledWith("repair", "^(a+)+$", "", {
    keep: "match",
  });
  expect(synckit.createSyncFn).toHaveBeenCalledTimes(1);
});

test("checkSync: pure", () => {
  syncBackend.mockReturnValueOnce("pure");

  const diagnostics = main.checkSync("^(a|a)+$", "");
  expect(diagnostics.status).toBe("vulnerable");
});

test("repair: java", async () => {
  backend.mockReturnValueOnce("java");
  javaEnsure.mockImplementationOnce(() =>
    agent.start("java", ["-jar", RECHECK_JAR, "agent"]),
  );

  const rx = await main.repair("^(a+)+$", "", { keep: "match-range" });
  expect(rx).toMatchObject({
    status: "repaired",
    repaired: "^(a)+$",
    kept: [0],
  });
});

test("repair: worker", async () => {
  backend.mockReturnValueOnce("worker");

  const rx = await main.repair("^(a+)+$", "", { keep: "match" });
  expect(rx).toEqual({ status: "safe", method: "repair", keep: "match" });
});

test("repair: pure", async () => {
  backend.mockReturnValueOnce("pure");

  const rx = await main.repair("^(a+)+$", "");
  expect(rx).toMatchObject({
    status: "repaired",
    repaired: "^(a+)$",
    kept: [0, 1],
  });
});

test("repairSync: pure", () => {
  syncBackend.mockReturnValueOnce("pure");

  const rx = main.repairSync("^(a+)+$", "", { keep: "match" });
  expect(rx).toMatchObject({
    status: "repaired",
    repaired: "^(a)+$",
    kept: [],
  });
});

test("checkSync: invalid", () => {
  syncBackend.mockReturnValueOnce("invalid" as any);

  expect(() => main.checkSync("^(a|a)+$", "")).toThrow(
    "invalid sync backend: invalid",
  );
});

// A runtime command line whose runtime attack always finishes, so that every attack is refuted.
const finishing =
  "sh -c 'echo recheck:runtime fake; echo recheck:start; echo recheck:time 0' sh";

test("check: java with refuted attacks", async () => {
  backend.mockReturnValueOnce("java");
  // Jest's `process.env` does not reach child processes, so `env` sets the variable.
  javaEnsure.mockImplementationOnce(() =>
    agent.start("env", [
      `RECHECK_NODE=${finishing}`,
      "java",
      "-jar",
      RECHECK_JAR,
      "agent",
    ]),
  );

  const diagnostics = await main.check("^(a|a)*$", "", {
    checker: "automaton",
    runtimeAttackTimeout: 1000,
  });
  expect(diagnostics).toMatchObject({
    status: "safe",
    unitMode: "utf16",
    refutedAttacks: [
      {
        complexity: { type: "exponential" },
        attack: { pattern: "'a'.repeat(31) + '\\x00'" },
        runtimeAttack: {
          runtime: "fake",
          timeout: 1000,
          result: { kind: "finish", time: 0 },
          isConfirmed: false,
        },
      },
    ],
  });
});
