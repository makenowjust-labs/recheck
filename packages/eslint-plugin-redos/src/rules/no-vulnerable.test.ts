import { RuleTester } from "eslint";

import rule from "./no-vulnerable";

const tester = new RuleTester({ parserOptions: { ecmaVersion: 2015 } });

tester.run("no-vulnerable", rule, {
  valid: [
    { code: `const x = /a/;` },
    {
      code: `const x = /^(a|a)*$/;`,
      options: [{ permittableComplexities: ["exponential"] }],
    },
    {
      code: `const x = /^a*a*$/;`,
      options: [{ permittableComplexities: ["polynomial"] }],
    },
    {
      code: `const x = /^a$/;`,
      options: [{ timeout: null }],
    },
    {
      code: `const x = /^a$/;`,
      options: [{ timeout: 1000 }],
    },
    {
      code: `const x = new NotRegExp('^(a|a)*$');`,
    },
    {
      code: `const x = NotRegExp('^(a|a)*$');`,
    },
    {
      code: `const x = /^(?=a*)$/; // ignoreErrors: true`,
      options: [{ checker: "automaton", ignoreErrors: true }],
    },
    { code: `const x = RegExp();` },
    { code: `const x = RegExp('', '', '');` },
    { code: `const x = RegExp(1, 2);` },
    {
      code: `const x = /^a$/;`,
      options: [
        {
          accelerationMode: "auto",
          attackLimit: 1500000000,
          attackTimeout: 1000,
          checker: "auto",
          crossoverSize: 25,
          heatRatio: 0.001,
          incubationLimit: 25000,
          incubationTimeout: 250,
          maxAttackStringSize: 300000,
          maxDegree: 4,
          maxGeneStringSize: 2400,
          maxGenerationSize: 100,
          maxInitialGenerationSize: 500,
          maxIteration: 10,
          maxNFASize: 35000,
          maxPatternSize: 1500,
          maxRecallStringSize: 300000,
          maxRepeatCount: 30,
          maxSimpleRepeatCount: 30,
          mutationSize: 50,
          randomSeed: 0,
          recallLimit: 1500000000,
          recallTimeout: -1000,
          seeder: "static",
          seedingLimit: 1000,
          seedingTimeout: 100,
          timeout: 10000,
        },
      ],
    },
    // TODO: Support template literals.
    { code: `const x = RegExp(\`^a*a*$\`);` },
  ],
  invalid: [
    {
      code: `const x = /^(a|a)*$/;`,
      errors: [{ message: "Found a ReDoS vulnerable RegExp (exponential)." }],
    },
    {
      code: `const x = /^a*a*$/;`,
      errors: [
        { message: "Found a ReDoS vulnerable RegExp (2nd degree polynomial)." },
      ],
    },
    {
      code: `const x = /^(?=a*)$/; // ignoreErrors: false`,
      options: [{ checker: "automaton", ignoreErrors: false }],
      errors: [
        {
          message:
            "Error on ReDoS vulnerability check: look-ahead assertion (unsupported)",
        },
      ],
    },
    {
      code: `const x = new RegExp('(');`,
      options: [{ ignoreErrors: false }],
      errors: [
        {
          message:
            "Error on ReDoS vulnerability check: parsing failure (at 1) (invalid)",
        },
      ],
    },
    {
      code: `const x = new RegExp('^(a|a)*$');`,
      errors: [{ message: "Found a ReDoS vulnerable RegExp (exponential)." }],
    },
    {
      code: `const x = new RegExp('^(a|a)*$', '');`,
      errors: [{ message: "Found a ReDoS vulnerable RegExp (exponential)." }],
    },
    {
      code: `const x = RegExp('^(a|a)*$');`,
      errors: [{ message: "Found a ReDoS vulnerable RegExp (exponential)." }],
    },
    {
      code: `const x = RegExp('^(a|a)*$', '');`,
      errors: [{ message: "Found a ReDoS vulnerable RegExp (exponential)." }],
    },
    {
      code: `const x = /^.*$/;`,
      options: [{ timeout: 0, ignoreErrors: false }],
      errors: [{ message: "Error on ReDoS vulnerability check: timeout" }],
    },
  ],
});
