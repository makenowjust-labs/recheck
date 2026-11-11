const { parentPort } = require("worker_threads");

parentPort.on("message", (message) => {
  const json = JSON.parse(message);
  const id = json.id;
  parentPort.postMessage(JSON.stringify({ id: -1, message: "message" }));
  parentPort.postMessage(JSON.stringify({ id, message: "message" }));
  setTimeout(() => {
    // A fake `repair` returns its method and `keep`.
    if (json.method === "repair") {
      const keep = json.params.keep ?? null;
      const result = { status: "safe", method: json.method, keep };
      parentPort.postMessage(JSON.stringify({ id, result }));
      return;
    }
    parentPort.postMessage(
      JSON.stringify({
        id,
        result: {
          source: "^(a|a)*$",
          flags: "",
          complexity: {
            type: "exponential",
            summary: "exponential",
            isFuzz: false,
          },
          status: "vulnerable",
          attack: {
            pattern: "'a'.repeat(31) + '\\x00'",
            string: "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\x00",
            base: 31,
            suffix: "\x00",
            pumps: [{ prefix: "", pump: "a", bias: 0 }],
          },
          checker: "automaton",
          hotspot: [
            { start: 2, end: 3, temperature: "heat" },
            { start: 4, end: 5, temperature: "heat" },
          ],
        },
      }),
    );
  }, 100);
});
