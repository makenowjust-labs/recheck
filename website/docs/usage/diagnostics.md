---
id: diagnostics
title: Diagnostics
---

`Diagnostics` is a result type of checking. In Scala, it is a `sealed class`.
But in TypeScript, it is a serialized object.
However, they have the same information. We will describe this.

## Common Fields

Every `Diagnostics` has the following fields.

- `source` is the given regular expression’s source string.
- `flags` is the given regular expression’s flags string.
- `dialect` is the given [`dialect`](parameters.mdx#dialect) parameter, as an object of the name and every setting of the dialect.
- `checker` is a checker type to be used (one of `fuzz` and `automaton`).

## Status

Known that already, `Diagnostics` has three statuses.

- `safe` means the given regular expression seems safe at least in this checking.
- `vulnerable` means vulnerability in the given regular expression is found.
- `unknown` means something wrong happened in checking (timeout, cancel, or error). It has the `error` field to describe the reason for this result.
  Its `kind` is one of `timeout`, `cancel`, `unsupported`, `invalid`, `recall-failure-limit` (see [Recall Validation](#recall-validation)) and `unexpected`.

## Complexity

`safe` and `vulnerable` has the `complexity` field.
This field contains the matching time complexity estimated in this checking.
There are five `complexity` varieties.

- `safe`, `constant`, and `linear` are safe matching time complexity.
- `polynomial` is a vulnerable matching time complexity. It has an additional field called `degree` which represents the maximum degree of the matching time complexity polynomial.
- `exponential` is a vulnerable matching time complexity.

:::caution

If the complexity is exponential, you must fix the regular expression as soon as possible.
It may invoke a matching time explosion against too little string (`length < 100`).

:::

## Attack String

`vulnerable` status diagnostics has the `attack` field. It contains the attack string obtained by this checking.

The attack string is a string to invoke the matching time explosion as known as ReDoS vulnerability.

It is a string with a repetition structure. It consists of `pumps`, `suffix`, and the `base` repetition count.
Each `pump` consists of `prefix`, `repeating` substrings, and the individual repetition count `n`. The actual string forms `prefix + repeating.repeat(base + n) + ... + suffix`.

The `attack` field also has these fields.

- `string` is the actual string. Its characters are UTF-16 code units, or bytes as the characters of the same codes (as Latin-1) when the pattern matches bytes.
- `pattern` is an expression that evaluates to the actual string in the host language of the [dialect](dialects.md).
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

Noting that the attack string is computed in theoretical, because of the real matching implementation’s optimization, it may not invoke the matching time explosion.
To prevent this case, you should use the recall validation.

## Recall Validation

`vulnerable` status diagnostics has the `recallValidation` field.

The recall validation runs the attack string on the real regex engine of the dialect (see [Recall Validation](recall-validation.md)), and this field records the run.
It is `null` if the recall validation is not run: [`recallTimeout`](parameters.mdx#recalltimeout) is not positive (the default), or commands cannot run (e.g. in the browser).

- `runtime` is the runtime name and version (e.g. `node v26.11.1`).
- `timeout` is the timeout of the matching in milliseconds (`null` if it is infinite).
- `result` is the result of the matching. Its `kind` is one of the following.
  - `finish` means the matching finished in `time` milliseconds, so the attack is not effective there.
  - `timeout` means the matching did not finish in time.
  - `engine-limit` means the engine stopped on its limit of the matching steps (PHP's backtrack limit), which also shows that the attack is effective. It has `message`.
  - `engine-error` means the engine gave up with an error on the stack depth or the memory (e.g. Java's `StackOverflowError` or JavaScript's `RangeError`). It has `message`.
    Such an error may happen on a long string even if the matching takes linear time (e.g. V8 raises it for `/^(?:a|b)*$/` on 10,000,000 characters), so the attack is not considered effective.
- `succeeded` is whether the attack is effective on the engine (`timeout` or `engine-limit`).

When the recall validation fails, the check does not report the diagnostics, and it goes on to look for another attack string.
If no attack succeeds, the result is `safe`.

`checkAll` (`ReDoS.checkAll` in Scala) returns the recall-failed diagnostics too, followed by the reported one.

## Hotspot

`hotspot` is another interesting field in `vulnerable` status diagnostics.

It shows a hotspot in the given regular expression.
A hotspot is a substring in the given regular expression that is executed many times against the attack string.

It helps you to fix the regular expression to a safe one.
