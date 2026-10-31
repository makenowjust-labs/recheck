---
id: javascript
title: JavaScript
---

Describes how `recheck` checks a regular expression of the `javascript` dialect, which is the default.

## Engine

`recheck` follows ECMAScript 2025 `RegExp`, as V8 implements it.

## Settings

The `javascript` dialect has no settings.

## Flags and Units

A pattern matches UTF-16 code units, or code points with the `u` or `v` flag.

The flags are the flags of `RegExp`.

| Flag | `RegExp` property | Inline     | Notes                                                                  |
| ---- | ----------------- | ---------- | ---------------------------------------------------------------------- |
| `d`  | `hasIndices`      |            | No effect on matching.                                                 |
| `g`  | `global`          |            | No effect on matching, since `recheck` models a search from the start. |
| `i`  | `ignoreCase`      | `(?i:...)` |                                                                        |
| `m`  | `multiline`       | `(?m:...)` |                                                                        |
| `s`  | `dotAll`          | `(?s:...)` |                                                                        |
| `u`  | `unicode`         |            | An error with `v`.                                                     |
| `v`  | `unicodeSets`     |            | An error with `u`.                                                     |
| `y`  | `sticky`          |            | The match starts at the start of the input (`lastIndex` is `0`).       |

JavaScript has no other options.

## Syntax

The syntax includes the ES2025 features: the `v` flag, the modifiers `(?i:...)` and duplicate named groups.

## Approximations

- V8 (Node.js 26) takes exponential time on `^(a|a)*$` against `'a'.repeat(n) + '!'`: about 2 seconds for `n = 24`.
- Case-insensitive matching uses JavaScript's `Canonicalize`.
- For the `v` flag, `recheck` follows the specification.
- A look-behind matches backward from the position.
- A back-reference sees the captures as JavaScript resets them on each iteration of a loop.
- An optional iteration matching the empty string is rejected.

## Recall Validation

- Command: `node -`, which reads the program from the standard input.
- Matching: `RegExp.prototype.exec`.
- Engine limits: none.
- Engine errors: `RangeError` (e.g. V8 raises it for `/^(?:a|b)*$/` on 10,000,000 characters).
- Environment variable: `RECHECK_NODE`.
- Recall script: `node recall.cjs`.

See [Recall Validation](../recall-validation.md) for how it works.
