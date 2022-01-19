---
id: diagnostics
title: Diagnostics
---

`Diagnostics` is a result type of checking. In Scala, it is a `sealed class`.
But in TypeScript, it is a serialized object.
However, they have the same information. We will describe this.

## Common Fields

All `Diagnostics` has the following fields.

- `source` is the given regular expression’s source string.
- `flags` is the given regular expression’s flags string.
- `checker` is a checker type to be used (one of `fuzz` and `automaton`).

## Status

Known that already, `Diagnostics` has three statuses.

- `safe` means the given regular expression seems safe at least in this checking.
- `vulnerable` means vulnerability in the given regular expression is found.
- `unknown` means something wrong happened in checking (timeout, cancel, or error). It has the `error` field to describe the reason for this result.

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

Noting that the attack string is computed in theoretical, because of the real matching implementation’s optimization, it may not invoke the matching time explosion.
To prevent this case, you should use the recall validation.

## Hotspot

`hotspot` is another interesting field in `vulnerable` status diagnostics.

It shows a hotspot in the given regular expression.
A hotspot is a substring in the given regular expression that is executed many times against the attack string.

It helps you to fix the regular expression to a safe one.
