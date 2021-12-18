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
            "Error on ReDoS vulnerablity check: look-ahead assertion (unsupported)",
        },
      ],
    },
    {
      code: `const x = new RegExp('(');`,
      options: [{ ignoreErrors: false }],
      errors: [
        {
          message:
            "Error on ReDoS vulnerablity check: parsing failure (at 1) (invalid)",
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
      errors: [{ message: "Error on ReDoS vulnerablity check: timeout" }],
    },
  ],
});
