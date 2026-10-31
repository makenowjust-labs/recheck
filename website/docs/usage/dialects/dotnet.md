---
id: dotnet
title: .NET
---

Describes how `recheck` checks a regular expression of the `dotnet` dialect.

## Engine

`recheck` follows the syntax and the semantics of .NET 10's `System.Text.RegularExpressions.Regex`.

## Settings

The `dotnet` dialect has no settings.

## Flags and Units

- A pattern matches UTF-16 code units.
- Case-insensitive matching uses `ToLowerInvariant` between single characters.
  It follows the invariant culture, not the current culture (e.g. `(?i)i` does not match `İ` as it does in the Turkish culture).

The flags are the letters of the inline options.

| Flag | `RegexOptions`            | Inline | Notes                                 |
| ---- | ------------------------- | ------ | ------------------------------------- |
| `i`  | `IgnoreCase`              | `(?i)` | In the invariant culture (see above). |
| `m`  | `Multiline`               | `(?m)` |                                       |
| `n`  | `ExplicitCapture`         | `(?n)` |                                       |
| `s`  | `Singleline`              | `(?s)` |                                       |
| `x`  | `IgnorePatternWhitespace` | `(?x)` |                                       |

The inline options also take upper case letters (e.g. `(?I)`), but the flags are lower case.

The `RegexOptions` that `recheck` does not take:

- `CultureInvariant`: always assumed, since `recheck` uses the invariant culture.
- `Compiled`: no effect on matching.
- `RightToLeft`: not supported.
- `ECMAScript`: not supported.
- `NonBacktracking`: not supported (the engine does not backtrack, and it matches in linear time in the length of the input).

## Syntax Limitations

The following constructs are not supported, and they are reported as `unsupported`.

- Balancing groups (e.g. `(?<close-open>...)`).
- Internal property names (e.g. `\p{_xmlI}`).

## Approximations

- .NET has optimizations that `recheck` does not model, so an attack that `recheck` reports may not work on .NET, and the recall validation checks whether it does.

## Recall Validation

- Command: `dotnet run <dir>/RecheckRecall-<hash>.cs`.
- Matching: `Regex.Match`.
- Engine limits: none.
- Engine errors: `OutOfMemoryException`, `InsufficientExecutionStackException`.
- Environment variable: `RECHECK_DOTNET`.
- Recall script: `dotnet run recall.cs`.

`dotnet` needs the .NET 10 SDK for `dotnet run` of a file.
It runs a fixed program reading the pattern and the attack string from the standard input, so `dotnet` builds and caches the program only once.
`recheck` writes the program file once into its directory in the temporary directory (see [Program Files](../recall-validation.md#program-files)), so a command in a container needs the directory mounted at the same path.
The program sets the invariant culture (`CultureInfo.CurrentCulture`) before compiling the pattern, so the result does not depend on the locale of the environment (e.g. `LC_ALL` or `LANG`).
The recall script has the pattern and the attack string in itself.
