import { spawn } from "child_process";

import type { ChildProcess } from "child_process";

import type { Diagnostics, HasAbortSignal, Parameters } from "../..";

/** Ref is a pair of promise `resolve`/`reject` functions, and `subscribe` handler of request. */
type Ref = {
  resolve: (value: any) => void;
  reject: (err: Error) => void;
  subscribe: ((message: any) => void) | null;
};

/**
 * Agent is a shallow `recheck agent` command wrapper.
 * It is JSON-RPC client via `child` process's stdio.
 */
export class Agent {
  /** A child process of `recheck agent`. */
  private readonly child: ChildProcess;

  /** The next request ID number. */
  private nextID: number;

  /** A mapping from request ID to corresponding promise reference. */
  private readonly refs: Map<number, Ref>;

  constructor(child: ChildProcess) {
    this.child = child;
    this.nextID = 0;
    this.refs = new Map();
    this.handle();
  }

  /** Sends a request to `recheck agent` process. */
  public request(
    method: string,
    params: any,
    subscribe: ((message: any) => void) | null = null
  ): { id: number; promise: Promise<any> } {
    const id = this.nextID++;
    const promise = new Promise((resolve, reject) => {
      const object = {
        jsonrpc: "2.0+push",
        id,
        method,
        params,
      };
      const text = JSON.stringify(object) + "\n";
      this.child.stdin!.write(text);
      this.registerRef(id, { resolve, reject, subscribe });
    });
    return { id, promise };
  }

  /** Sends a notification to `recheck agent` process. */
  public notify(method: string, params: any): Promise<void> {
    return new Promise((resolve) => {
      const object = {
        jsonrpc: "2.0+push",
        method,
        params,
      };
      const text = JSON.stringify(object) + "\n";
      this.child.stdin!.write(text);
      resolve();
    });
  }

  /** Sets up response handlers to `recheck agent` process. */
  private handle(): void {
    // Holds the remaining last line of the response.
    let remainingLastLine = "";

    const handleLine = (line: string) => {
      if (line === "") {
        return;
      }

      const { id, message, result } = JSON.parse(line);
      const ref = this.refs.get(id) ?? null;
      if (ref === null) {
        return;
      }

      if (message != undefined) {
        ref.subscribe?.(message);
        return;
      }

      if (result !== undefined) {
        ref.resolve(result);
        this.unregisterRef(id);
      }
    };

    this.child.stdout!.on("data", (data: Buffer) => {
      const text = data.toString("utf-8");
      const lines = text.split("\n");
      const hasNewline = lines.length > 1;

      /* c8 ignore next 1 */
      const lastLine = lines.pop() ?? "";
      const firstLine = lines.shift() ?? "";

      if (hasNewline) {
        lines.unshift(remainingLastLine + firstLine)
        remainingLastLine = '';
      }

      for (const line of lines) {
        handleLine(line);
      }

      remainingLastLine += lastLine;
    });
  }

  /** Registers the given reference. */
  private registerRef(id: number, ref: Ref): void {
    this.refs.set(id, ref);

    if (this.refs.size === 1) {
      this.child.ref();
    }
  }

  /** Removes a registration of the given ID reference. */
  private unregisterRef(id: number): void {
    this.refs.delete(id);

    if (this.refs.size === 0) {
      this.child.unref();
    }
  }

  /** Kills the `agent` process. */
  public kill(): void {
    this.child.unref();
    this.child.kill();
  }
}

/** Starts `recheck agent` with the given command and command-line arguments. */
export async function start(
  command: string,
  args: string[] = []
): Promise<Agent> {
  return await new Promise((resolve, reject) => {
    const child = spawn(command, args, {
      windowsHide: true,
      stdio: ["pipe", "pipe", "ignore"],
    });

    const onClose = () => reject(new Error("process is closed"));
    child.on("error", reject);
    child.on("close", onClose);

    // Remove references from the child process.
    child.unref();
    /* c8 ignore next 6 */
    if ((child.stdin as any)?.unref) {
      (child.stdin as any).unref();
    }
    if ((child.stdout as any)?.unref) {
      (child.stdout as any).unref();
    }

    // Waits `ping` method response.
    const agent = new Agent(child);
    agent
      .request("ping", {})
      .promise.then(() => {
        child.off("error", reject);
        child.off("close", onClose);
        resolve(agent);
      })
      .catch(reject);
  });
}

export async function check(
  agent: Agent,
  source: string,
  flags: string,
  params: Parameters & HasAbortSignal = {}
): Promise<Diagnostics> {
  // Drops `signal` parameter from `params`.
  const signal = params.signal ?? null;
  if (signal) {
    delete params.signal;
  }

  // Drops `logger` parameter from `params`.
  const logger = params.logger ?? null;
  if (logger) {
    params.logger = {} as any;
  }

  // Sends `'check'` method request.
  const { id, promise } = agent.request(
    "check",
    { source, flags, params },
    logger
  );

  if (signal?.aborted) {
    // Sends `'cancel'` when the signal is already aborted.
    agent.notify("cancel", { id });
  } else {
    // Adds `'abort'` signal handler.
    signal?.addEventListener("abort", () => {
      agent.notify("cancel", { id });
    });
  }

  return await promise;
}
