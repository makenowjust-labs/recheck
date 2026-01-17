import type { ESLint } from "eslint";

import pkg from "../package.json";

import noVulnerable from "./rules/no-vulnerable";
import recommended from "./configs/recommended";

const plugin = {
  meta: {
    name: pkg.name,
    version: pkg.version,
  },
  rules: {
    "no-vulnerable": noVulnerable,
  },
  configs: { recommended },
};

Object.assign(plugin.configs, {
  flat: {
    recommended: {
      ...recommended,
      plugins: {
        redos: plugin,
      },
    },
  },
});

export = plugin as ESLint.Plugin;
