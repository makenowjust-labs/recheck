import { Rule } from "eslint";
import type ESTree from "estree";
import * as ReDoS from "recheck";

type Options = {
  ignoreErrors: boolean;
  permittableComplexities?: ("polynomial" | "exponential")[];
  timeout?: number | null;
  checker?: "hybrid" | "automaton" | "fuzz";
};

const rule: Rule.RuleModule = {
  meta: {
    type: "problem",
    docs: {
      description: "disallow ReDoS vulnerable RegExp literals",
    },
    schema: [
      {
        properties: {
          ignoreErrors: {
            type: "boolean",
          },
          permittableComplexities: {
            type: "array",
            items: {
              enum: ["polynomial", "exponential"],
            },
            additionalItems: false,
            uniqueItems: true,
          },
          timeout: {
            type: ["number", "null"],
            minimum: 0,
          },
          checker: {
            type: "string",
            enum: ["hybrid", "automaton", "fuzz"],
          },
        },
        additionalProperties: false,
      },
    ],
  },
  create: (context) => {
    const options: Options = context.options[0] || {};
    const {
      ignoreErrors = true,
      permittableComplexities = [],
      timeout = 10000,
      checker = "hybrid",
    } = options;
    const config = { timeout: timeout ?? undefined, checker };

    const check = (node: ESTree.Node, source: string, flags: string) => {
      const result = ReDoS.checkSync(source, flags, config);
      switch (result.status) {
        case "safe":
          break;
        case "vulnerable":
          if (
            result.complexity &&
            permittableComplexities.includes(result.complexity?.type)
          ) {
            break;
          }
          switch (result.complexity?.type) {
            case "exponential":
            case "polynomial":
              context.report({
                message: `Found a ReDoS vulnerable RegExp (${result.complexity.summary}).`,
                node,
              });
              break;
            case undefined:
              context.report({
                message: "Found a ReDoS vulnerable RegExp.",
                node,
              });
              break;
          }
          break;
        case "unknown":
          if (ignoreErrors) {
            break;
          }
          switch (result.error.kind) {
            case "timeout":
              context.report({
                message: `Error on ReDoS vulnerablity check: timeout`,
                node,
              });
              break;
            case "invalid":
            case "unsupported":
              context.report({
                message: `Error on ReDoS vulnerablity check: ${result.error.message} (${result.error.kind})`,
                node,
              });
              break;
          }
      }
    };

    return {
      Literal: (node) => {
        // Tests `/.../`?
        if (!(node.value instanceof RegExp)) {
          return;
        }

        const { source, flags } = node.value;
        check(node, source, flags);
      },
      NewExpression: (node) => {
        // Tests `new RegExp(...)`?
        if (
          !(node.callee.type === "Identifier" && node.callee.name === "RegExp")
        ) {
          return;
        }
        // TODO: Support template literals.
        // Tests `new RegExp(...)` or `new RegExp(..., ...)`?
        if (!(node.arguments.length == 1 || node.arguments.length == 2)) {
          return;
        }
        // Tests `new RegExp('...', '...')`?
        if (
          !node.arguments.every(
            (arg) => arg.type === "Literal" && typeof arg.value === "string"
          )
        ) {
          return;
        }

        const [source, flags = ""] = node.arguments.map(
          (arg) => (arg as ESTree.Literal).value as string
        );
        check(node, source, flags);
      },
      CallExpression: (node) => {
        // Tests `RegExp(...)`?
        if (
          !(node.callee.type === "Identifier" && node.callee.name === "RegExp")
        ) {
          return;
        }
        // TODO: Support template literals.
        // Tests `RegExp(...)` or `RegExp(..., ...)`?
        if (!(node.arguments.length == 1 || node.arguments.length == 2)) {
          return;
        }
        // Tests `RegExp('...', '...')`?
        if (
          !node.arguments.every(
            (arg) => arg.type === "Literal" && typeof arg.value === "string"
          )
        ) {
          return;
        }

        const [source, flags = ""] = node.arguments.map(
          (arg) => (arg as ESTree.Literal).value as string
        );
        check(node, source, flags);
      },
    };
  },
};

export = rule;
