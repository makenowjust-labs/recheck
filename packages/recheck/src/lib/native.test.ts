import { expect, jest, test } from "@jest/globals";

jest.unstable_mockModule("./agent", () => ({
  start: jest.fn(),
  check: jest.fn(),
}));
jest.unstable_mockModule("./exe", () => ({ jar: jest.fn(), bin: jest.fn() }));

const agent = await import("./agent");
const exe = await import("./exe");
const native = await import("./native");

test("ensure: bin === null", async () => {
  jest.mocked(exe.bin).mockReturnValueOnce(null);

  await expect(native.ensure()).resolves.toBeNull();
  expect(exe.bin).toHaveBeenCalled();
});

test("ensure: bin !== null", async () => {
  jest.mocked(exe.bin).mockReturnValueOnce("x");
  const fake: any = Symbol();
  jest.mocked(agent.start).mockReturnValueOnce(fake);

  await expect(native.ensure()).resolves.toBe(fake);
  expect(exe.bin).toHaveBeenCalled();
  expect(agent.start).toHaveBeenCalledWith("x", ["agent"]);
});
