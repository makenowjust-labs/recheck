---
id: dotnet
title: .NET
---

Describes how `recheck` checks a regular expression of the `dotnet` dialect.
See [Dialects](../dialects.md) for what is common to all the dialects.

## Engine

`recheck` follows .NET 10's `System.Text.RegularExpressions.Regex`.

## Settings

The `dotnet` dialect has no settings.

## Flags and Units

- A pattern matches UTF-16 code units.
- Case-insensitive matching uses `ToLowerInvariant` between single characters.
  It follows the invariant culture, not the current culture (e.g. `(?i)i` does not match `İ` as it does in the Turkish culture).

## Syntax

The following constructs are not supported, and they are reported as `unsupported`.

- Balancing groups (e.g. `(?<close-open>...)`).
- Internal property names (e.g. `\p{_xmlI}`).

## Approximations

- .NET 10 finishes `^(a|a)*$` against `'a'.repeat(n) + '!'` in less than 1 ms even for `n = 28`, and `recheck` does not model the optimizations making it fast.
- A look-behind matches backward from the position.
- An iteration matching the empty string ends the repetition when it is the last mandatory one or an optional one (e.g. `^(?:()|a){2}\1$` sets the group `1` at the start of `a` in .NET and Python, but at the end in Java and Perl).
- The captures in a negative look-around condition are restored after it.
- .NET does not capture the first unnamed group after a conditional with a look-around (e.g. `(?(?=(a))a)(b)` captures `b` as the group `1`, and the group `2` is never set), as its parser does.

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

See [Recall Validation](../recall-validation.md) for how it works.
