let count = 0;
let checks = {};
let methods = {};

process.stdin.on("data", (data) => {
  const lines = data.toString("utf-8").split("\n").filter(Boolean);
  for (line of lines) {
    const json = JSON.parse(line);
    switch (json.method) {
      case "ping":
        // Tests to ignore unexpected inputs.
        console.log();
        console.log(
          JSON.stringify({ jsonrpc: "2.0+push", id: json.id - 1, result: {} }),
        );

        console.log(
          JSON.stringify({ jsonrpc: "2.0+push", id: json.id, result: {} }),
        );
        break;
      case "test-request":
        console.log(
          JSON.stringify({
            jsonrpc: "2.0+push",
            id: json.id,
            message: "message",
          }),
        );
        console.log(
          JSON.stringify({ jsonrpc: "2.0+push", id: json.id, result: count }),
        );
        break;
      case "test-notify":
        count += 1;
        break;

      // A fake `checker` implementation.
      case "check":
      case "checkAll":
        console.log(
          JSON.stringify({
            jsonrpc: "2.0+push",
            id: json.id,
            message: "message",
          }),
        );
        const wrap = (result) =>
          json.method === "checkAll" ? [result] : result;
        methods[json.id] = json.method;
        checks[json.id] = setTimeout(() => {
          switch (json.params.source) {
            case "test-large":
              const string = "a".repeat(300_000);
              console.log(
                JSON.stringify({
                  jsonrpc: "2.0+push",
                  id: json.id,
                  result: wrap({ status: "vulnerable", attack: { string } }),
                }),
              );
              break;
            default:
              console.log(
                JSON.stringify({
                  jsonrpc: "2.0+push",
                  id: json.id,
                  result: wrap({ status: "safe" }),
                }),
              );
              break;
          }
          delete checks[json.id];
        }, 100);
        break;
      case "cancel":
        console.log(
          JSON.stringify({
            jsonrpc: "2.0+push",
            id: json.params.id,
            result:
              methods[json.params.id] === "checkAll"
                ? [{ status: "unknown" }]
                : { status: "unknown" },
          }),
        );
        clearTimeout(checks[json.params.id]);
        delete checks[json.params.id];
        break;
    }
  }
});
