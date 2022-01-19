const { Worker: WorkerThread } = require("worker_threads");

module.exports = function createWorker() {
  const thread = new WorkerThread(`${__dirname}/test-worker-script.js`);
  return {
    addEventListener(name, callback) {
      thread.on("message", (data) => callback({ data }));
    },
    postMessage(message) {
      thread.postMessage(JSON.stringify(message));
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
  };
};
