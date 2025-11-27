import * as synckit from "synckit";

import * as main from "./main";

import * as env from "./lib/env";
import * as java from "./lib/java";
import * as native from "./lib/native";

const RECHECK_JAR = `${__dirname}/../../../modules/recheck-cli/target/scala-3.7.4/recheck.jar`;
const RECHECK_BIN = `${__dirname}/../../../modules/recheck-cli/target/native-image/recheck`;

jest.setTimeout(30 * 1000);

jest.mock("synckit");
jest.mock("./lib/env");
jest.mock("./lib/java");
jest.mock("./lib/native");

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
  const backend = jest.spyOn(env, "RECHECK_BACKEND");
  backend.mockReturnValueOnce("auto");
  const jar = jest.spyOn(env, "RECHECK_JAR");
  jar.mockReturnValueOnce(RECHECK_JAR);
  const ensure = jest.spyOn(java, "ensure");
  ensure.mockImplementationOnce(jest.requireActual("./lib/java").ensure);

  const diagnostics = await main.check("^(a|a)+$", "");
  expect(diagnostics.status).toBe("vulnerable");
  expect(ensure).toHaveBeenCalled();
});

test("check: auto (fallback)", async () => {
  const backend = jest.spyOn(env, "RECHECK_BACKEND");
  backend.mockReturnValueOnce("auto");
  const javaEnsure = jest.spyOn(java, "ensure");
  javaEnsure.mockResolvedValueOnce(null);
  const nativeEnsure = jest.spyOn(native, "ensure");
  nativeEnsure.mockResolvedValueOnce(null);

  const diagnostics = await main.check("^(a|a)+$", "");
  expect(diagnostics.status).toBe("vulnerable");
  expect(javaEnsure).toHaveBeenCalled();
  expect(nativeEnsure).toHaveBeenCalled();
});

test("check: auto (fallback, error)", async () => {
  const backend = jest.spyOn(env, "RECHECK_BACKEND");
  backend.mockReturnValueOnce("auto");
  const javaEnsure = jest.spyOn(java, "ensure");
  javaEnsure.mockRejectedValueOnce(new Error("java.ensure error"));
  const nativeEnsure = jest.spyOn(native, "ensure");
  nativeEnsure.mockRejectedValueOnce(new Error("native.ensure error"));

  const diagnostics = await main.check("^(a|a)+$", "");
  expect(diagnostics.status).toBe("vulnerable");
  expect(javaEnsure).toHaveBeenCalled();
  expect(nativeEnsure).toHaveBeenCalled();
});

test("check: java (1)", async () => {
  const backend = jest.spyOn(env, "RECHECK_BACKEND");
  backend.mockReturnValueOnce("java");
  const jar = jest.spyOn(env, "RECHECK_JAR");
  jar.mockReturnValueOnce(RECHECK_JAR);
  const ensure = jest.spyOn(java, "ensure");
  ensure.mockImplementationOnce(jest.requireActual("./lib/java").ensure);

  const diagnostics = await main.check("^(a|a)+$", "");
  expect(diagnostics.status).toBe("vulnerable");
  expect(ensure).toHaveBeenCalled();
});

test("check: java (2)", async () => {
  const backend = jest.spyOn(env, "RECHECK_BACKEND");
  backend.mockReturnValueOnce("java");
  const ensure = jest.spyOn(java, "ensure");
  ensure.mockResolvedValueOnce(null);

  expect(main.check("^(a|a)+$", "")).rejects.toThrow(
    "there is no available implementation",
  );
});

// This test is skipped because the native binary is not available on CI environment.
test.skip("check: native (1)", async () => {
  const backend = jest.spyOn(env, "RECHECK_BACKEND");
  backend.mockReturnValueOnce("native");
  const bin = jest.spyOn(env, "RECHECK_BIN");
  bin.mockReturnValueOnce(RECHECK_BIN);
  const ensure = jest.spyOn(native, "ensure");
  ensure.mockImplementationOnce(jest.requireActual("./lib/native").ensure);

  const diagnostics = await main.check("^(a|a)+$", "");
  expect(diagnostics.status).toBe("vulnerable");
  expect(ensure).toHaveBeenCalled();
});

test("check: native (2)", async () => {
  const backend = jest.spyOn(env, "RECHECK_BACKEND");
  backend.mockReturnValueOnce("native");
  const ensure = jest.spyOn(native, "ensure");
  ensure.mockResolvedValueOnce(null);

  expect(main.check("^(a|a)+$", "")).rejects.toThrow(
    "there is no available implementation",
  );
});

test("check: worker", async () => {
  const backend = jest.spyOn(env, "RECHECK_BACKEND");
  backend.mockReturnValueOnce("worker");

  const diagnostics = await main.check("^(a|a)+$", "");
  expect(diagnostics.status).toBe("vulnerable");
});

test("check: pure", async () => {
  const backend = jest.spyOn(env, "RECHECK_BACKEND");
  backend.mockReturnValueOnce("pure");

  const diagnostics = await main.check("^(a|a)+$", "");
  expect(diagnostics.status).toBe("vulnerable");
});

test("check: invalid", async () => {
  const backend = jest.spyOn(env, "RECHECK_BACKEND");
  backend.mockReturnValueOnce("invalid" as any);

  expect(main.check("^(a|a)+$", "")).rejects.toThrow(
    "invalid backend: invalid",
  );
});

test("checkSync: synckit", () => {
  const backend = jest.spyOn(env, "RECHECK_SYNC_BACKEND");
  backend.mockReturnValueOnce("synckit" as any);
  const createSyncFn = jest.spyOn(synckit, "createSyncFn");
  createSyncFn.mockReturnValueOnce(() => ({ status: "vulnerable" }));

  const diagnostics = main.checkSync("^(a|a)+$", "");
  expect(diagnostics.status).toBe("vulnerable");
  expect(createSyncFn).toHaveBeenCalled();
});

test("checkSync: pure", () => {
  const backend = jest.spyOn(env, "RECHECK_SYNC_BACKEND");
  backend.mockReturnValueOnce("pure" as any);

  const diagnostics = main.checkSync("^(a|a)+$", "");
  expect(diagnostics.status).toBe("vulnerable");
});

test("checkSync: invalid", () => {
  const backend = jest.spyOn(env, "RECHECK_SYNC_BACKEND");
  backend.mockReturnValueOnce("invalid" as any);

  expect(() => main.checkSync("^(a|a)+$", "")).toThrow(
    "invalid sync backend: invalid",
  );
});
