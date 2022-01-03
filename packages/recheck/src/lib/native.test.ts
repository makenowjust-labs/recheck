import * as agent from "./agent";
import * as exe from "./exe";

import * as native from "./native";

jest.mock("./agent");
jest.mock("./exe");

test("ensure: bin === null", () => {
  const bin = jest.spyOn(exe, "bin");
  bin.mockReturnValueOnce(null);

  expect(native.ensure()).resolves.toBeNull();
  expect(bin).toHaveBeenCalled();
});

test("ensure: jar !== null", () => {
  const bin = jest.spyOn(exe, "bin");
  bin.mockReturnValueOnce("x");

  const start = jest.spyOn(agent, "start");
  const fake: any = Symbol();
  start.mockReturnValueOnce(fake);

  expect(native.ensure()).resolves.toBe(fake);
  expect(bin).toHaveBeenCalled();
  expect(start).toHaveBeenCalledWith("x", ["agent"]);
});
