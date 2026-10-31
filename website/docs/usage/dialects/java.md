---
id: java
title: Java
---

Describes how `recheck` checks a regular expression of the `java` dialect.
See [Dialects](../dialects.md) for what is common to all the dialects.

## Engine

`recheck` follows Java 25's `java.util.regex.Pattern`.

## Settings

The `java` dialect has no settings.

## Flags and Units

- A pattern matches code points, where a lone surrogate is a code point too.
- The flags `U` (`UNICODE_CHARACTER_CLASS`) and `u` (`UNICODE_CASE`) change the meaning of `\w`, `\d`, `\s`, `\b`, POSIX classes and case-insensitive matching in the same way as in `java.util.regex`.
- `\b` without `U` also takes a non-spacing mark after a letter or a digit as a word character (e.g. `\b` does not match between `a` and U+0301), as `java.util.regex` does.

## Syntax

The canonical equivalence (`(?c)`) is not supported, and it is reported as `unsupported`.

## Approximations

- Java's optimizations of loops make some vulnerable patterns fast, and `recheck` does not model them.
  Java 25 finishes `^(a|a)*$` against `'a'.repeat(n) + '!'` in less than 1 ms even for `n = 28`.
- A look-behind matches forward from a start before the position, and the match must end at the position.
  Java tries the starts from the nearest by the lengths that its `TreeInfo` computes, in UTF-16 code units, or in code points when the pattern has a supplementary character after the look-behind.
- A look-behind with `\X` matches nowhere if its `TreeInfo` lengths leave no start to try (e.g. `(?<=\X)`).
- Java does not restore the captures in an atomic group, a look-around or a repeated group without branches (e.g. `(?:(b)c)*`) once it matches, even when it backtracks to a later starting position (e.g. `(?:(a)b)+x|b\1` matches `ba` in `aba`), and it matches each iteration of such a group, or of `\R` under a quantifier, in its first way.
- An optional iteration matching the empty string is rejected in a repetition of a single node or of a group without branches.
  A repetition of a group with branches ends with any iteration matching the empty string, even before the minimum count (e.g. `^(?:()|a){2}\1$` sets the group `1` at the end of `a` in Java and Perl, but at the start in Python and .NET).

## Recall Validation

- Command: `java --source 17 <dir>/RecheckRecall-<hash>.java`.
- Matching: `Matcher#find`.
- Engine limits: none.
- Engine errors: `StackOverflowError`, `OutOfMemoryError`.
- Environment variable: `RECHECK_JAVA`.
- Recall script: `java --source 17 recall.java`.

`java` runs a fixed program as a single-file source program, which needs a JDK (17 or later).
The program reads the pattern and the attack string from the standard input.
`recheck` writes the program file once into its directory in the temporary directory (see [Program Files](../recall-validation.md#program-files)), so a command in a container needs the directory mounted at the same path.
The recall script has the pattern and the attack string in itself.

See [Recall Validation](../recall-validation.md) for how it works.
