/** A port to communicate to the parent process. */
export const port = {
  onMessage(callback: (message: string) => void): void {
    global.addEventListener("message", ({ data }) => callback(data));
  },
  postMessage(message: string): void {
    global.postMessage(message);
  },
};
