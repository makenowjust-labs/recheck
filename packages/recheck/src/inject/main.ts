import { Worker as WorkerThread } from "worker_threads";

// This file provides fake `Blob`, `URL` and `Worker` to run a worker with an inline script.

/** Returns a given `data` immediately. */
export function Blob(data: string): string {
  // NOTE: `Blob` is called with `new`, so it will be `String`.
  return data;
}

export const URL = {
  /** Converts `data` to a primitive string. */
  createObjectURL(data: String): string {
    return String(data);
  },
  /** Does nothing. */
  revokeObjectURL(data: string): void {
    return;
  },
};

/**
 * Returns a worker to run a given `data` as a script.
 *
 * It will returns a Web Worker like object.
 * It works well on `recheck` implementation at least.
 */
export function Worker(data: string): Worker {
  const thread = new WorkerThread(data, { eval: true });

  return {
    addEventListener(name: "message", callback: (data: any) => void): void {
      thread.on("message", (data) => callback({ data }));
    },
    postMessage(message: string): void {
      thread.postMessage(message);
    },
    terminate() {
      thread.terminate();
    },
    ref() {
      thread.ref();
    },
    unref() {
      thread.unref();
    },
  } as any as Worker;
}
