/** A port to communicate to the parent process. */
export const port = {
  onMessage(callback: (message: string) => void): void {
    self.addEventListener("message", ({ data }) => callback(data));
  },
  postMessage(message: string): void {
    self.postMessage(message);
  },
};
