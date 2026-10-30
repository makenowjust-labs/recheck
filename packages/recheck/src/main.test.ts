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

test("checkSync: synckit", () => {
  syncBackend.mockReturnValueOnce("synckit");
  const createSyncFn = jest.mocked(synckit.createSyncFn);
  createSyncFn.mockReturnValueOnce((() => ({ status: "vulnerable" })) as any);

  const diagnostics = main.checkSync("^(a|a)+$", "");
  expect(diagnostics.status).toBe("vulnerable");
  expect(createSyncFn).toHaveBeenCalled();
});

test("checkSync: pure", () => {
  syncBackend.mockReturnValueOnce("pure");

  const diagnostics = main.checkSync("^(a|a)+$", "");
  expect(diagnostics.status).toBe("vulnerable");
});

test("checkSync: invalid", () => {
  syncBackend.mockReturnValueOnce("invalid" as any);

  expect(() => main.checkSync("^(a|a)+$", "")).toThrow(
    "invalid sync backend: invalid",
  );
});
