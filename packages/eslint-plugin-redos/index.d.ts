import type { Rule, Config, FlatConfig } from "eslint";

declare const plugin: {
  meta: {
    name: string;
    version: string;
  },
  rules: {
    "no-vulnerable": Rule.RuleModule;
  },
  configs: {
    recommended: Config;
    flat: {
      recommended: FlatConfig;
    };
  };
};

export = plugin;
