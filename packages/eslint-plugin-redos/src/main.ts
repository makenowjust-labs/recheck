import noVulnerable from "./rules/no-vulnerable";
import recommended from "./configs/recommended";

export = {
  rules: {
    "no-vulnerable": noVulnerable,
  },
  configs: { recommended },
};
