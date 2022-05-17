import { Rule } from "eslint";
import type ESTree from "estree";
import * as ReDoS from "recheck";

type Options = {
  ignoreErrors: boolean;
  permittableComplexities?: ("polynomial" | "exponential")[];
} & Omit<ReDoS.Parameters, "logger">;

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
          accelerationMode: {
            type: "string",
            enum: ["auto", "on", "off"],
          },
          attackLimit: {
            type: "number",
          },
          attackTimeout: {
            type: ["number", "null"],
          },
          checker: {
            type: "string",
            enum: ["auto", "automaton", "fuzz"],
          },
          crossoverSize: {
            type: "number",
          },
          heatRatio: {
            type: "number",
          },
          incubationLimit: {
            type: "number",
          },
          incubationTimeout: {
            type: ["number", "null"],
          },
          maxAttackStringSize: {
            type: "number",
          },
          maxDegree: {
            type: "number",
          },
          maxGeneStringSize: {
            type: "number",
          },
          maxGenerationSize: {
            type: "number",
          },
          maxInitialGenerationSize: {
            type: "number",
          },
          maxIteration: {
            type: "number",
          },
          maxNFASize: {
            type: "number",
          },
          maxPatternSize: {
            type: "number",
          },
          maxRecallStringSize: {
            type: "number",
          },
          maxRepeatCount: {
            type: "number",
          },
          maxSimpleRepeatCount: {
            type: "number",
          },
          mutationSize: {
            type: "number",
          },
          randomSeed: {
            type: "number",
          },
          recallLimit: {
            type: "number",
          },
          recallTimeout: {
            type: ["number", "null"],
          },
          seeder: {
            type: "string",
            enum: ["static", "dynamic"],
          },
          seedingLimit: {
            type: "number",
          },
          seedingTimeout: {
            type: ["number", "null"],
          },
          timeout: {
            type: ["number", "null"],
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
      ...params
    } = options;

    const check = (node: ESTree.Node, source: string, flags: string) => {
      const result = ReDoS.checkSync(source, flags, { timeout, ...params });
      switch (result.status) {
        case "safe":
          break;
        case "vulnerable":
          if (permittableComplexities.includes(result.complexity.type)) {
            break;
          }
          switch (result.complexity.type) {
            case "exponential":
            case "polynomial":
              context.report({
                message: `Found a ReDoS vulnerable RegExp (${result.complexity.summary}).`,
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

    const isCallOrNewRegExp = (
      node: ESTree.NewExpression | ESTree.CallExpression
    ) => {
      // Tests `RegExp(...)` or `new RegExp(...)`?
      if (
        !(node.callee.type === "Identifier" && node.callee.name === "RegExp")
      ) {
        return false;
      }
      // Tests `RegExp(...)`, `RegExp(..., ...)`?
      if (!(node.arguments.length == 1 || node.arguments.length == 2)) {
        return false;
      }
      // TODO: Support template literals.
      // Tests `RegExp('...', '...')`?
      if (
        !node.arguments.every(
          (arg) => arg.type === "Literal" && typeof arg.value === "string"
        )
      ) {
        return false;
      }

      return true;
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
        if (!isCallOrNewRegExp(node)) {
          return;
        }

        const [source, flags = ""] = node.arguments.map(
          (arg) => (arg as ESTree.Literal).value as string
        );
        check(node, source, flags);
      },
      CallExpression: (node) => {
        if (!isCallOrNewRegExp(node)) {
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
