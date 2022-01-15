# eslint-plugin-redos

> [ESLint][] plugin for catching [ReDoS][] vulnerability.

[![npm (scoped)](https://img.shields.io/npm/v/eslint-plugin-redos?logo=javascript&style=for-the-badge)](https://www.npmjs.com/package/eslint-plugin-redos)

[eslint]: https://eslint.org
[redos]: https://en.wikipedia.org/wiki/ReDoS

## Installation

```console
$ npm install eslint-plugin-redos
```

Then, in your `.eslintrc.json`:

```json
{
  "plugins": ["redos"],
  "rules": {
    "redos/no-vulnerable": "error"
  }
}
```

This plugin contains the only rule `redos/no-vulnerable`.

## Note

[`recheck`](https://makenowjust-labo.github.io/recheck) is a ReDoS vulnerability checker used by this plugin. It is very optimized and faster enough, however it takes seconds in some cases. This time is essential because ReDoS detection is not so easy problem in computer science.

Therefore, to reduce unnecessary computation time, we recommend adding such a following settings.

```json
{
  "plugins": ["redos"],
  "rules": {
    "redos/no-vulnerable": "error"
  },
  "overrides": [
    {
      "files": ["**/*.test.{js,ts}"],
      "rules": {
        "redos/no-vulnerable": "off"
      }
    }
  ]
}
```

The above settings disables the `redos/no-vulnerable` rule against test files. Since ReDoS vulnerabilities in test codes are not critical problems, it will be no problem in many cases.

Alternatively, you can use rules that use lightweight (but imperfect) analysis methods such as [`regexp/no-super-linear-backtracking`](https://ota-meshi.github.io/eslint-plugin-regexp/rules/no-super-linear-backtracking.html) included in `eslint-plugin-regexp` during development, and use this plugin in CI.

---

Disallow ReDoS vulnerable RegExp literals \
(`redos/no-vulnerable`)
===

## Rule Details

Almost all JavaScript's `RegExp` matching engines are backtrack based,
and it is known that such an engine causes [ReDoS][] (**Regular Expression Denial of Service**) vulnerability.
This rule detects a RegExp literal causing problematic backtracking behavior potentially.

:-1: Examples of **incorrect** code for this rule:

```javascript
/*eslint redos/no-vulnerable: "error"*/

// Exponential times backtracking examples:
/^(a*)*$/;
/^(a|a)*$/;
/^(a|b|ab)*$/;

// Polynomial times backtracking examples:
/^a*a*$/;
/^[\s\u200c]+|[\s\u200c]+$/; // See https://stackstatus.net/post/147710624694/outage-postmortem-july-20-2016.
```

:+1: Examples of **correct** code for this rule:

```javascript
/*eslint redos/no-vulnerable: "error"*/

// Fixed times backtracking examples:
/^a$/;
/^foo$/;

// Linear times backtracking examples;
/foo/;
/(a*)*/;
```

## Options

The following is the default configuration.

```json
{
  "redos/no-vulnerable": [
    "error",
    {
      "ignoreErrors": true,
      "permittableComplexities": [],
      "timeout": 10000,
      "checker": "auto"
    }
  ]
}
```

### `ignoreErrors`

This flag is used to determine to ignore errors on ReDoS vulnerable detection.

Errors on ReDoS vulnerable detection are:

- the pattern is invalid.
- the pattern is not supported to analyze.
- analysis is timeout.

They are ignored because they are noisy usually.

### `permittableComplexity`

This array option controls permittable matching complexity.
It allows the following values.

- `'polynomial'`
- `'exponential'`

We strongly recommend considering `'polynomial'` matching complexity RegExp as ReDoS vulnerable.
However, this option can disable it.

### `timeout`

This option specifies a time-out limit for ReDoS analyzing.
A time-unit is milli-seconds.
If `null` is specified, it means unlimited time-out.

The default value is `10000` (10 seconds).

### `checker`

This option specifies a checker name to use.
It is one of `'auto'`, `'automaton'` and `'fuzz'`.

See [the `recheck` documentation](https://github.com/MakeNowJust-Labo/recheck/blob/main/packages/recheck/README.md) for detailed informations.

The default value is `'hybrid'`.

## Related Projects

- [`recheck`](https://makenowjust-labo.github.io/recheck): a ReDoS detection library used in this plugin.

---

## License

MIT license.

2020-2022 (C) TSUYUSATO "MakeNowJust" Kitsune
