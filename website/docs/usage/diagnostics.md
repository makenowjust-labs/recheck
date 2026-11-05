---
id: diagnostics
title: Diagnostics
---

`Diagnostics` is what `check` reports about a regular expression.
It is not a final verdict on whether the regular expression is a threat.
It is the evidence for judging the ReDoS risk: whether an attack string was found, the estimated complexity, the attack string itself, and how they were found.
Whether a finding matters depends on where the regular expression is used, e.g. how long an input can reach it and which engine runs it.
So the decision is left to you, or to the tool using `recheck` (e.g. the [ESLint plugin](as-eslint-plugin.mdx) decides it by its options).

In Scala, `Diagnostics` is a `sealed class`, and in TypeScript, it is a serialized object.
They have the same information, which this page describes.

## Common Fields

Every `Diagnostics` has the following fields.

- `source` is the given regular expression’s source string.
- `flags` is the given regular expression’s flags string.
- `dialect` is the given [`dialect`](parameters.mdx#dialect) parameter, as an object of the name and every setting of the dialect.
- `unitMode` is the unit of the attack strings (see [Matching Units](dialects.mdx#matching-units)).
  It is `null` in `unknown` diagnostics when the error happened on parsing the pattern.
- `refutedAttacks` is the list of the attack strings that the runtime-attack verification refuted before the result (see [Runtime-Attack Verification](#runtime-attack-verification)).
- `checker` is a checker type to be used (one of `fuzz` and `automaton`).

## Status

`Diagnostics` has three statuses.

- `safe` means that the checking found no attack string within its limits.
  The fuzz checker may miss an attack string, so `safe` diagnostics from it (`complexity.isFuzz` is `true`) are weaker evidence than those from the automaton checker.
- `vulnerable` means that the checking found an attack string, which is the evidence of the vulnerability (see [Attack String](#attack-string)).
  When the runtime-attack verification is enabled, the attack string is also confirmed on the real engine (see [Runtime-Attack Verification](#runtime-attack-verification)).
- `unknown` means that the checking could not finish (timeout, cancel, or error), so it gives no evidence either way. It has the `error` field to describe the reason for this result.
  Its `kind` is one of `timeout`, `cancel`, `unsupported`, `invalid`, `runtime-attacks-refuted` (see [Runtime-Attack Verification](#runtime-attack-verification)) and `unexpected`.

## Complexity

`safe` and `vulnerable` have the `complexity` field.
This field contains the matching time complexity estimated in this checking.
There are five `complexity` varieties.

- `safe`, `constant`, and `linear` are safe matching time complexities.
  `safe` means that the checking found no attack string, but it did not estimate the complexity.
- `polynomial` is a vulnerable matching time complexity. It has an additional field called `degree`, which is the degree of the polynomial.
  The degree shows how fast the matching time grows: e.g. with degree 2, an input 10 times longer takes about 100 times longer.
- `exponential` is a vulnerable matching time complexity.

Every complexity has the `isFuzz` field.
It is `true` when the fuzz checker estimated the complexity by running the matching, which may be wrong, and `false` when the automaton checker analysed it.

:::caution

If the complexity is exponential, you must fix the regular expression as soon as possible.
It may invoke a matching time explosion against too little string (`length < 100`).

:::

## Attack String

An attack string is a string that makes the matching time explode, which is the evidence of the ReDoS vulnerability.
`vulnerable` diagnostics have it in the `attack` field.

It has a repetition structure: it consists of `pumps`, `suffix`, and the `base` repetition count.
Each element of `pumps` has `prefix`, the repeated substring `pump`, and its own repetition count `bias`.
The actual string is `prefix + pump.repeat(base + bias) + ... + suffix`.

The `attack` field also has these fields.

- `string` is the actual string. Its characters are UTF-16 code units, or bytes as the characters of the same codes (as Latin-1) when `unitMode` is `byte`.
- `pattern` is an expression that evaluates to the actual string in the host language of the [dialect](dialects.mdx).
  The attack string of `/^(a|a)*$/` looks like the following.

  | Dialect      | Language   | `attack.pattern`                                                               |
  | ------------ | ---------- | ------------------------------------------------------------------------------ |
  | `javascript` | JavaScript | `'a'.repeat(31) + '\x00'`                                                      |
  | `python`     | Python     | `'a' * 31 + '\x00'`, or `b'a' * 31 + b'\x00'` for `bytes`                      |
  | `ruby`       | Ruby       | `"a" + "a" * 31 + "\x00"`, or `"a".b + "a".b * 31 + "\x00".b` for `ASCII-8BIT` |
  | `java`       | Java       | `"a".repeat(31) + "\u0000"`                                                    |
  | `dotnet`     | C#         | `string.Concat(Enumerable.Repeat("a", 31)) + "\u0000"` (`System.Linq`)         |
  | `perl`       | Perl       | `"a" x 31 . "\x00"`                                                            |
  | `pcre`       | PHP        | `str_repeat('a', 31) . "\x00"`                                                 |

Note that the attack string is computed in theory, so it may not cause the explosion on the real engine because of the engine's optimizations.
The runtime-attack verification checks it on the real engine.

## Runtime-Attack Verification

`vulnerable` diagnostics have the `runtimeAttack` field.

The runtime-attack verification runs the attack string on the real regex engine of the dialect (see [Runtime-Attack Verification](runtime-attack-verification.mdx)), and this field records the run.
It is `null` if the runtime attack is not run: [`runtimeAttackTimeout`](parameters.mdx#runtimeattacktimeout) is not positive (the default), or commands cannot run (e.g. in the browser).

- `runtime` is the runtime name and version (e.g. `node v26.11.1`).
- `timeout` is the timeout of the matching in milliseconds (`null` if it is infinite).
- `result` is the result of the matching. Its `kind` is one of the following.
  - `finish` means the matching finished in `time` milliseconds, so the attack is not effective there.
  - `timeout` means the matching did not finish in time.
  - `engine-limit` means the engine stopped on its limit of the matching steps (PHP's backtrack limit), which also shows that the attack is effective. It has `message`.
  - `engine-error` means the engine gave up with an error on the stack depth or the memory (e.g. Java's `StackOverflowError` or JavaScript's `RangeError`). It has `message`.
    Such an error may happen on a long string even if the matching takes linear time (e.g. V8 raises it for `/^(?:a|b)*$/` on 10,000,000 characters), so the attack is not considered effective.
- `isConfirmed` is whether the engine confirms the attack, that is, the attack is effective on the engine (`timeout` or `engine-limit`).
  Otherwise, the runtime-attack verification refutes the attack.

When the runtime-attack verification refutes an attack, the check does not report the diagnostics, and it goes on to look for another attack string.
If no attack is confirmed, the result is `safe`.

The diagnostics of every status list the refuted attacks in the `refutedAttacks` field, in the order of the checking.
Each element has the `complexity`, `attack`, `hotspot` and `runtimeAttack` fields of the refuted attack, which are the same as those of `vulnerable` diagnostics.
They show that the pattern is vulnerable on a naive backtracking engine, though the dialect's engine is not affected by these attacks (see [Refuted Attacks](runtime-attack-verification.mdx#refuted-attacks)).

## Hotspot

`hotspot` is another interesting field in `vulnerable` status diagnostics.

It shows a hotspot in the given regular expression.
A hotspot is a substring in the given regular expression that is executed many times against the attack string.

It helps you to fix the regular expression to a safe one.
