import * as agent from "./agent";
import * as exe from "./exe";

import * as java from "./java";

jest.mock("./agent");
jest.mock("./exe");

test("ensure: jar === null", () => {
  const jar = jest.spyOn(exe, "jar");
  jar.mockReturnValueOnce(null);

  expect(java.ensure()).resolves.toBeNull();
  expect(jar).toHaveBeenCalled();
});

test("ensure: jar !== null", () => {
  const jar = jest.spyOn(exe, "jar");
  jar.mockReturnValueOnce("x");

  const start = jest.spyOn(agent, "start");
  const fake: any = Symbol();
  start.mockReturnValueOnce(fake);

  expect(java.ensure()).resolves.toBe(fake);
  expect(jar).toHaveBeenCalled();
  expect(start).toHaveBeenCalledWith("java", ["-jar", "x", "agent"]);
});
