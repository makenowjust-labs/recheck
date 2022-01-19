import { parentPort } from "worker_threads";

/** A port to communicate to the parent process. */
export const port = {
  onMessage(callback: (message: string) => void): void {
    parentPort!.on("message", callback);
  },
  postMessage(message: string): void {
    parentPort!.postMessage(message);
  },
};
