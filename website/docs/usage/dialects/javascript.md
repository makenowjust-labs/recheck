---
id: javascript
title: JavaScript
---

Describes how `recheck` checks a regular expression of the `javascript` dialect, which is the default.

## Engine

`recheck` follows the syntax and the semantics of ECMAScript 2025 `RegExp`, as V8 implements it.

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

## Approximations

- With the `y` flag, the match starts only at the start of the input, as with `lastIndex` of `0`.
  A match from a later `lastIndex` is not modeled, so `recheck` misses a pattern that is slow only there, where a look-behind, `\b`, `\B` or a multiline `^` sees the text before the starting position (e.g. `/(?<=a)(?:b|b)*$/y` with `lastIndex` of `1` on `'a' + 'b'.repeat(n) + '!'`).

## Recall Validation

- Command: `node -`, which reads the program from the standard input.
- Matching: `RegExp.prototype.exec`.
- Engine limits: none.
- Engine errors: `RangeError` (e.g. V8 raises it for `/^(?:a|b)*$/` on 10,000,000 characters).
- Environment variable: `RECHECK_NODE`.
- Recall script: `node recall.cjs`.
