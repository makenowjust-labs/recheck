import * as pure from "./pure";

declare global {
  /** A port to communicate to the parent process. */
  var port: {
    onMessage(callback: (message: string) => void): void;
    postMessage(message: string): void;
  };
}

// Sets a handler to check.
port.onMessage((message) => {
  const request = JSON.parse(message);
  let logger: ((message: string) => void) | undefined = undefined;
  if (request.params.logger !== undefined) {
    logger = (message: string) => {
      port.postMessage(JSON.stringify({ id: request.id, message }));
    };
  }
  const result = pure.check(request.source, request.flags, {
    ...request.params,
    logger,
  });
  port.postMessage(JSON.stringify({ id: request.id, result }));
});

// The beloe two lines are dummy to pass a type checking.
declare function createWorker(): Worker;
export { createWorker as default };
