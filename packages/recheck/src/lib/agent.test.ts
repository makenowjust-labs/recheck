import { StdioNull, StdioPipe } from "child_process";
import { check as checkAgent, start as startAgent, Agent } from "./agent";

const invalid = `${__dirname}/__test__/invalid.js`;
const testAgent = `${__dirname}/__test__/test-agent.js`;

let agent: Agent | null = null;
const start = async (
  command: string,
  args: string[] = [],
  stdio: (StdioPipe | StdioNull)[] = ["pipe", "pipe", "inherit"],
) => {
  agent = await startAgent(command, args, stdio);
  return agent;
};

afterEach(() => {
  agent?.kill();
  agent = null;
});

test("start", async () => {
  await expect(start("node", [testAgent])).resolves.toBeInstanceOf(Agent);
});

test("start: invalid", async () => {
  await expect(
    start("node", [invalid], ["pipe", "pipe", "ignore"]),
  ).rejects.toThrowError();
});

test("Agent#request", async () => {
  const agent = await start("node", [testAgent]);
  const { id, promise } = agent.request("test-request", {});
  expect(id).toBe(1);
  await expect(promise).resolves.toBe(0);
});

test("Agent#request: with message", async () => {
  expect.assertions(3);

  const agent = await start("node", [testAgent]);
  const subscribe = (message: string) => expect(message).toBe("message");
  const { id, promise } = agent.request("test-request", {}, subscribe);
  expect(id).toBe(1);
  await expect(promise).resolves.toBe(0);
});

test("Agent#notify", async () => {
  const agent = await start("node", [testAgent]);
  await agent.notify("test-notify", {});
  await expect(agent.request("test-request", {}).promise).resolves.toBe(1);
});

test("check", async () => {
  const agent = await start("node", [testAgent]);
  await expect(checkAgent(agent, "test", "")).resolves.toEqual({
    status: "safe",
  });
});

test("check: with large output", async () => {
  const agent = await start("node", [testAgent]);
  await expect(checkAgent(agent, "test-large", "")).resolves.toEqual({
    status: "vulnerable",
    attack: { string: "a".repeat(300_000) },
  });
});

test("check: with logger", async () => {
  expect.assertions(2);

  const agent = await start("node", [testAgent]);
  const logger = (message: string) => expect(message).toBe("message");
  await expect(checkAgent(agent, "test", "", { logger })).resolves.toEqual({
    status: "safe",
  });
});

test("check: with abort (1)", async () => {
  const agent = await start("node", [testAgent]);
  const controller = new AbortController();
  const signal = controller.signal;
  controller.abort();
  await expect(checkAgent(agent, "test", "", { signal })).resolves.toEqual({
    status: "unknown",
  });
});

test("check: with abort (2)", async () => {
  const agent = await start("node", [testAgent]);
  const controller = new AbortController();
  const signal = controller.signal;
  const run = checkAgent(agent, "test", "", { signal });
  setTimeout(() => controller.abort(), 10);
  await expect(run).resolves.toEqual({ status: "unknown" });
});
