---
id: java
title: Java
---

Describes how `recheck` checks a regular expression of the `java` dialect.

## Engine

`recheck` follows the syntax and the semantics of Java 25's `java.util.regex.Pattern`.

## Settings

The `java` dialect has no settings.

## Flags and Units

- A pattern matches code points, where a lone surrogate is a code point too.
- The flags `U` (`UNICODE_CHARACTER_CLASS`) and `u` (`UNICODE_CASE`) change the meaning of `\w`, `\d`, `\s`, `\b`, POSIX classes and case-insensitive matching in the same way as in `java.util.regex`.
- `\b` without `U` also takes a non-spacing mark after a letter or a digit as a word character (e.g. `\b` does not match between `a` and U+0301), as `java.util.regex` does.

The flags are the letters of the inline flags of `java.util.regex`.

| Flag | `Pattern` flag            | Inline | Notes                                                          |
| ---- | ------------------------- | ------ | -------------------------------------------------------------- |
| `c`  | `CANON_EQ`                | `(?c)` | Not supported (see [Syntax Limitations](#syntax-limitations)). |
| `d`  | `UNIX_LINES`              | `(?d)` | Only `\n` is a line terminator for `.`, `^` and `$`.           |
| `i`  | `CASE_INSENSITIVE`        | `(?i)` | Only between ASCII characters, unless with `u` or `U`.         |
| `m`  | `MULTILINE`               | `(?m)` |                                                                |
| `s`  | `DOTALL`                  | `(?s)` |                                                                |
| `u`  | `UNICODE_CASE`            | `(?u)` |                                                                |
| `x`  | `COMMENTS`                | `(?x)` |                                                                |
| `U`  | `UNICODE_CHARACTER_CLASS` | `(?U)` | It implies `UNICODE_CASE`.                                     |

The `Pattern` flags that `recheck` does not take:

- `LITERAL`: not supported (it has no inline flag); `Pattern.quote` makes a pattern matching the same strings.

## Syntax Limitations

The canonical equivalence (`(?c)`) is not supported, and it is reported as `unsupported`.

## Approximations

- Java has optimizations of loops that `recheck` does not model, so an attack that `recheck` reports may not work on Java, and the recall validation checks whether it does.

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
