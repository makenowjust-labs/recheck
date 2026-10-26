import { createRequire } from "node:module";

// The CommonJS bundle has no `import.meta.url` and uses its own `require`, while the ES module bundle and the tests resolve from this file.
export const nodeRequire: NodeJS.Require = import.meta.url
  ? createRequire(import.meta.url)
  : require;
