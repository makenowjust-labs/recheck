import { expect, jest, test } from "@jest/globals";

jest.unstable_mockModule("./agent", () => ({
  start: jest.fn(),
  check: jest.fn(),
}));
jest.unstable_mockModule("./exe", () => ({ jar: jest.fn(), bin: jest.fn() }));

const agent = await import("./agent");
const exe = await import("./exe");
const java = await import("./java");

test("ensure: jar === null", async () => {
  jest.mocked(exe.jar).mockReturnValueOnce(null);

  await expect(java.ensure()).resolves.toBeNull();
  expect(exe.jar).toHaveBeenCalled();
});

test("ensure: jar !== null", async () => {
  jest.mocked(exe.jar).mockReturnValueOnce("x");
  const fake: any = Symbol();
  jest.mocked(agent.start).mockReturnValueOnce(fake);

  await expect(java.ensure()).resolves.toBe(fake);
  expect(exe.jar).toHaveBeenCalled();
  expect(agent.start).toHaveBeenCalledWith("java", ["-jar", "x", "agent"]);
});
