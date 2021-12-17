// This file provides the `recheck agent` wrapper implementation.

import { spawn } from "child_process";
import { debuglog } from "util";

import type { ChildProcess } from "child_process";

import type { Diagnostics, HasAbortSignal, Parameters } from "..";

const debug = debuglog("recheck-agent");

/** A mapping from a supported platform (OS) name to the corresponding package name component. */
const osNames: Record<string, string> = {
  darwin: "macos",
  linux: "linux",
  win32: "windows",
};

/** A mapping from a supported architecture (CPU) name to the corresponding package name component. */
const cpuNames: Record<string, string> = {
  x64: "x64",
};

/** Returns a `recheck` CLI path if possible. */
const getCLIPath = (): string | null => {
  // If `RECHECK_PATH` environment variable is set, then it returns this immediately.
  const RECHECK_PATH = process.env["RECHECK_PATH"] || null; // `||` is intentional for ignoring an empty string.
  if (RECHECK_PATH !== null) {
    return RECHECK_PATH;
  }

  // Fetches `os` and `cpu` name. If it is not supported, then it returns `null`.
  const os = osNames[process.platform];
  const cpu = cpuNames[process.arch];
  const isWin32 = os === "windows";
  if (!os || !cpu) {
    return null;
  }

  try {
    // Constructs a package name with a binary file name, and resolves this.
    // If it is succeeded, we expect that the result path is `recheck` CLI.
    const bin = isWin32 ? "recheck.exe" : "recheck";
    const pkg = `recheck-${os}-${cpu}/${bin}`;
    const path = require.resolve(pkg);
    return path;
  } catch (err: any) {
    if (err && err.code == "MODULE_NOT_FOUND") {
      return null;
    }

    throw err;
  }
};

/** Ref is a pair of promise `resolve`/`reject` functions. */
type Ref = {
  resolve: (value: any) => void;
  reject: (err: Error) => void;
  subscribe: (message: any) => void;
};

/**
 * Agent is a shallow `recheck agent` command wrapper.
 * It is JSON-RPC client via `child` process's stdio.
 */
class Agent {
  /** A child process of `recheck agent`. */
  private readonly child: ChildProcess;

  /** The next request ID number. */
  private id: number;

  /** A mapping from request ID to corresponding promise reference. */
  private readonly refs: Map<number, Ref>;

  constructor(child: ChildProcess) {
    this.child = child;
    this.id = 0;
    this.refs = new Map();
    this.handle();
  }

  /** Sends a request to `recheck agent` process. */
  public request(
    method: string,
    params: any,
    subscribe: (message: any) => void
  ): { id: number; promise: Promise<any> } {
    const id = this.id++;
    const promise = new Promise((resolve, reject) => {
      const object = {
        jsonrpc: "2.0+push",
        id,
        method,
        params,
      };
      const text = JSON.stringify(object) + "\n";
      debug("Send a request: %s", text);
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
      debug("Send a notification: %s", text);
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

      debug("Handle a response line: %s", line);
      const { id, message, result } = JSON.parse(line);
      const ref = this.refs.get(id) ?? null;
      if (ref === null) {
        debug("A promise reference is missing: %s", id);
        return;
      }

      if (message != undefined) {
        ref.subscribe(message);
        return;
      }

      if (result !== undefined) {
        ref.resolve(result);
        this.unregisterRef(id);
      }
    };

    this.child.stdout!.on("data", (data) => {
      const text = data.toString("utf-8");
      const lines = text.split("\n");

      const lastLine = lines.pop() ?? "";
      const firstLine = lines.shift() ?? "";

      handleLine(remainingLastLine + firstLine);
      for (const line of lines) {
        handleLine(line);
      }

      remainingLastLine = lastLine;
    });
  }

  /** Registers the given reference. */
  private registerRef(id: number, ref: Ref): void {
    this.refs.set(id, ref);

    if (this.refs.size === 1) {
      debug("A agent process is referenced.");
      this.child.ref();
    }
  }

  /** Removes a registration of the given ID reference. */
  private unregisterRef(id: number): void {
    this.refs.delete(id);

    if (this.refs.size === 0) {
      debug("A agent process is unreferenced.");
      this.child.unref();
    }
  }
}

/** The running agent. */
let agent: Agent | null = null;

/** Returns running agent if possible, or it returns `null`. */
export function ensureAgent(): Agent | null {
  if (agent !== null) {
    debug("Running agent is found.");
    return agent;
  }

  const cli = getCLIPath();
  debug("`recheck` CLI path: %s", cli);
  if (cli === null) {
    return null;
  }

  // TODO: Handle failures on process spawning.
  debug("Run `%s agent`.", cli);
  const child = spawn(cli, ["agent"], {
    windowsHide: true,
    stdio: ["pipe", "pipe", "inherit"],
  });
  debug("Agent PID: %d", child.pid);

  // Remove references from the child process.
  child.unref();
  if ((child.stdin as any)?.unref) {
    (child.stdin as any).unref();
  }
  if ((child.stdout as any)?.unref) {
    (child.stdout as any).unref();
  }

  agent = new Agent(child);
  return agent;
}

export async function check(
  source: string,
  flags: string,
  params: Parameters & HasAbortSignal = {}
): Promise<Diagnostics> {
  const signal = params.signal ?? null;
  if (signal) {
    delete params.signal;
  }

  const agent = ensureAgent();
  if (agent === null) {
    throw new Error("`recheck` command is missing.");
  }

  const logger = params.logger ?? ((message: string) => {});
  if (params?.logger) {
    params.logger = [] as any;
  }

  const { id, promise } = agent.request(
    "check",
    { source, flags, params },
    logger
  );
  signal?.addEventListener("abort", () => {
    agent.notify("cancel", { id });
  });

  return await promise;
}
