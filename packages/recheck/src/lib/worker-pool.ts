import createWorker from "./worker";

import type { Diagnostics, HasAbortSignal, Parameters } from "../..";

/** Ref is an object contains a check information to resolve or to cancel. */
type Ref = {
  id: number;
  source: string;
  flags: string;
  params: Parameters;
  resolve: (value: any) => void;
  reject: (err: Error) => void;
  subscribe: ((message: any) => void) | null;
  signal: AbortSignal | null;
  worker: Worker | null;
  onabort: (() => void) | null;
};

/** WorkerPool is a pool of checker workers. */
export class WorkerPool {
  /** A maximum upper limit of running workers. */
  private readonly maxWorkerSize: number;

  /** A size number of current running workers. */
  private runningWorkerSize: number = 0;
  /** A list of workers not being checking. */
  private readonly freeWorkers: Worker[] = [];

  /** A mapping from request ID to corresponding reference. */
  private readonly refs: Map<number, Ref> = new Map();
  /** A queue for waiting to check. */
  private readonly queue: Ref[] = [];

  /** The next request ID number. */
  private nextID: number = 0;

  constructor(maxWorkerSize: number = 1) {
    this.maxWorkerSize = maxWorkerSize;
  }

  /** Checks a given regular expression using this pool. */
  public check(
    source: string,
    flags: string,
    params: Parameters & HasAbortSignal = {},
  ): Promise<Diagnostics> {
    const id = this.nextID++;

    const newParams = { ...params };

    // Drops `signal` parameter from `newParams`.
    const signal = newParams.signal ?? null;
    if (signal) {
      delete newParams.signal;
    }

    // Drops `logger` parameter from `newParams`.
    const logger = newParams.logger ?? null;
    if (logger) {
      newParams.logger = {} as any;
    }

    return new Promise((resolve, reject) => {
      // Creates and registers a reference.
      const ref = {
        id,
        source,
        flags,
        params: newParams,
        resolve,
        reject,
        subscribe: logger,
        signal,
        worker: null,
        onabort: null,
      };
      this.refs.set(id, ref);
      this.queue.push(ref);

      this.wakeUp();
    });
  }

  /** Kills this pool workers. */
  public kill(): void {
    this.runningWorkerSize = 0;
    for (const ref of this.refs.values()) {
      this.cancelRef(ref);
    }
    for (const worker of this.freeWorkers) {
      worker.terminate();
    }
  }

  /** Consumes the waiting queue if possible. */
  private wakeUp(): void {
    while (this.queue.length > 0) {
      // Gets or creates a free worker.
      let worker = this.freeWorkers.shift();
      if (worker === undefined) {
        if (this.runningWorkerSize < this.maxWorkerSize) {
          worker = this.createWorker();
        }
      }

      // If there is no free worker, it breaks this loop.
      if (worker === undefined) {
        break;
      }

      // Gets a waiting reference and removes it from the queue.
      const ref = this.queue.shift()!;

      // If a reference is aborted, this is cancelled and the worker keeps free.
      if (ref.signal?.aborted) {
        this.cancelRef(ref);
        this.freeWorkers.unshift(worker);
        continue;
      }

      // Uses the free worker.
      // Calls `ref` to prevent to terminate the Node process.
      if ((worker as any).ref) {
        (worker as any).ref();
      }
      ref.worker = worker;

      // Posts a check request.
      worker.postMessage(
        JSON.stringify({
          id: ref.id,
          source: ref.source,
          flags: ref.flags,
          params: ref.params,
        }),
      );

      // Registers an `abort` handler.
      if (ref.signal) {
        ref.onabort = () => this.cancelRef(ref);
        ref.signal.addEventListener("abort", ref.onabort);
      }
    }
  }

  private createWorker(): Worker {
    // Increases a number of running workers, and creates a worker.
    this.runningWorkerSize += 1;
    const worker = createWorker();

    // Sets a handler to the worker.
    worker.addEventListener("message", ({ data }) => {
      const { id, message, result } = JSON.parse(data);

      const ref = this.refs.get(id) ?? null;
      if (ref === null) {
        return;
      }

      if (message !== undefined) {
        ref.subscribe?.(message);
        return;
      }

      if (result !== undefined) {
        ref.resolve(result);
        this.unregisterRef(ref);

        if ((worker as any).unref) {
          (worker as any).unref();
        }
        this.freeWorkers.push(worker);

        this.wakeUp();
      }
    });

    // Calls `unref` on Node to terminate the Node process correctly.
    if ((worker as any).unref) {
      (worker as any).unref();
    }

    return worker;
  }

  /** Cancels the given refernece. */
  private cancelRef(ref: Ref): void {
    ref.resolve({
      source: ref.source,
      flags: ref.flags,
      status: "unknown",
      error: { kind: "cancel" },
    });

    this.unregisterRef(ref);
    if (ref.worker) {
      this.runningWorkerSize -= 1;
      ref.worker.terminate();
    }

    this.wakeUp();
  }

  /** Removes a reference from this pool. */
  private unregisterRef(ref: Ref): void {
    this.refs.delete(ref.id);

    if (ref.signal && ref.onabort) {
      ref.signal.removeEventListener("abort", ref.onabort);
    }
  }
}
