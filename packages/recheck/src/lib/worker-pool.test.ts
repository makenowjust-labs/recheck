import { WorkerPool } from "./worker-pool";

let pool: WorkerPool | null = null;
const start = (size: number = 1) => {
  pool = new WorkerPool(size);
  return pool;
};

afterEach(() => {
  pool?.kill();
});

test("check", async () => {
  const pool = start();

  const diagnostics = await pool.check("^(a|a)*$", "");
  expect(diagnostics.status).toBe("vulnerable");
});

test("check: with logger", async () => {
  expect.assertions(2);

  const pool = start();

  const logger = (message: string) => expect(message).toBe("message");
  const diagnostics = await pool.check("^(a|a)*$", "", { logger });
  expect(diagnostics.status).toBe("vulnerable");
});

test("check: with abort (1)", async () => {
  const pool = start();

  const controller = new AbortController();
  const signal = controller.signal;
  controller.abort();
  const diagnostics = await pool.check("^(a|a)*$", "", { signal });
  expect(diagnostics.status).toBe("unknown");
});

test("check: with abort (2)", async () => {
  const pool = start();

  const controller = new AbortController();
  const signal = controller.signal;
  const promise = pool.check("^(a|a)*$", "", { signal });
  setTimeout(() => controller.abort(), 10);
  expect((await promise).status).toBe("unknown");
});

test("check: parallel", async () => {
  const pool = start(2);

  const promises = [];
  for (let i = 0; i < 3; i++) {
    promises.push(pool.check("^(a|a)*$", ""));
  }
  const dianostices = await Promise.all(promises);
  expect(dianostices.map((d) => d.status)).toEqual([
    "vulnerable",
    "vulnerable",
    "vulnerable",
  ]);
});

test("kill", async () => {
  const pool = start();
  const promise = pool.check("^(a|a)*$", "");
  pool.kill();
  expect((await promise).status).toBe("unknown");
});
