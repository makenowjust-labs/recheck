---
id: dialects
title: Dialects
---

Describes the regular expression dialects that `recheck` supports, and how each of them is checked.

## Overview

`recheck` checks a regular expression in the syntax and the semantics of one dialect.
The [`dialect`](parameters.mdx#dialect) parameter chooses it, and its default is `javascript`.
The flags are the dialect's own flag letters (e.g. `imsx`), not a JavaScript flags string.

| `dialect`    | Engine that `recheck` follows                  | Matching unit                                      | Recall validation command     |
| ------------ | ---------------------------------------------- | -------------------------------------------------- | ----------------------------- |
| `javascript` | ECMAScript 2025 `RegExp` (V8)                  | UTF-16 code units, or code points with `u` or `v`  | `node -`                      |
| `python`     | Python 3.15 `re`                               | code points (`str`), or bytes (`bytes`)            | `python3 -I -`                |
| `ruby`       | Ruby 4.0 `Regexp` (Onigmo)                     | code points (`UTF-8`), or bytes (`ASCII-8BIT`)     | `ruby -`                      |
| `java`       | Java 25 `java.util.regex.Pattern`              | code points                                        | `java --source 17 /dev/stdin` |
| `dotnet`     | .NET 10 `System.Text.RegularExpressions.Regex` | UTF-16 code units                                  | `dotnet run -`                |
| `perl`       | Perl 5.44 regular expressions                  | code points (`characters`), or bytes (`bytes`)     | `perl -`                      |
| `pcre`       | PCRE2 10.49 as PHP's `preg_*` functions use it | bytes, or code points (UTF-8) with `u` or `(*UTF)` | `php`                         |

The following shows how to check a pattern in the Python dialect.

```bash
recheck --dialect=python '/^(a|a)*$/'
```

```tsx
import { check } from "recheck";

await check("^(a|a)*$", "", { dialect: "python" });
```

```scala
import codes.quine.labs.recheck.ReDoS
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.resyntax.ast.Dialect

ReDoS.check("^(a|a)*$", "", Parameters(dialect = Dialect.Python))
```

The Playground also has a selector of the dialect, and it shows `encoding`, `perlSource` and `perlUtf8` for the dialects having them.

:::note

The ESLint plugin (`eslint-plugin-redos`) checks JavaScript regular expressions only, so it has no `dialect` option.

:::

## Syntax

`recheck` reads the whole syntax of the latest version of each engine in the table above.
It also reproduces the errors that the engine reports after reading the syntax, as far as possible: for example, the restriction on the look-behind length, a reference to a missing group, and an unknown Unicode property name.
A pattern that the engine rejects is reported as `unknown` with an `invalid` error.

For JavaScript, it includes the ES2025 features: the `v` flag, the modifiers `(?i:...)` and duplicate named groups.
For Python, it includes atomic groups and possessive quantifiers (Python 3.11 or later).

Some constructs are recognized, but the checkers do not handle them.
A pattern having one of them is reported as `unknown` with an `unsupported` error naming the construct (e.g. `(a)?(?(1)b|c)` gives `unsupported syntax (Conditional) at 4`).

- Recursion and subroutine calls (e.g. `(?R)`, `(?1)`, `(?&name)`, `\g<name>`).
- Conditionals (e.g. `(?(1)a|b)`).
- Backtracking control verbs (e.g. `(*SKIP)`, `(*FAIL)`) and PCRE's `(*NOTEMPTY)` and `(*NOTEMPTY_ATSTART)`.
- Callouts (e.g. `(?C1)`) and embedded code (e.g. Perl's `(?{ ... })` and `(??{ ... })`).
- `\G`, `\K`, `\R`, `\X`, `\N{name}`, and `\C` in a mode other than bytes.
- Word boundaries of a kind, such as `\b{wb}`.
- Back-references with a nest level (e.g. Ruby's `\k<n+1>`).
- .NET's balancing groups (e.g. `(?<close-open>...)`).
- Ruby's absence operators (e.g. `(?~abc)`).
- Non-atomic look-arounds (e.g. PCRE's `(*napla:...)`), PCRE's `(*scan_substring:...)`, and script runs (e.g. `(*sr:...)`).
- Java's canonical equivalence (`(?c)`).
- Unicode properties that `recheck` has no data for (e.g. Perl's `\p{Line_Break=...}`).

Perl's and PCRE's extended bracketed character classes (`(?[ ... ])`) are not supported yet, and they are reported as `invalid`.

## Units and Encodings

A pattern matches a string of UTF-16 code units, of code points, or of bytes, which depends on the dialect.
The dialects having the choice take the [`encoding`](parameters.mdx#encoding) parameter, and its values are the dialect's own words.

- `javascript`: UTF-16 code units, or code points with the `u` or `v` flag. It has no `encoding`.
- `python`: `str` (default) matches code points, and `bytes` matches bytes. The flag `b` also makes a `bytes` pattern.
- `ruby`: `UTF-8` (default) matches code points, and `ASCII-8BIT` matches bytes. The flag `n` also makes an `ASCII-8BIT` pattern.
- `java`: code points, where a lone surrogate is a code point too. It has no `encoding`.
- `dotnet`: UTF-16 code units. It has no `encoding`.
- `perl`: `characters` (default) matches code points as the character strings with `use utf8`, and `bytes` matches bytes, where a non-ASCII literal is its UTF-8 bytes.
- `pcre`: bytes, or code points of UTF-8 with the `u` flag or `(*UTF)`, as in PHP. It has no `encoding`. As in PHP, `u` also makes `\d`, `\w` and the like Unicode classes.

An encoding that the dialect does not have makes the result `unknown` with an `invalid` error.

The attack string (`attack.pattern` of the diagnostics) is written in the JavaScript notation for every dialect, e.g. `'a'.repeat(31) + '\x00'`.
In a bytes mode, each byte is shown as the character of the same code (as Latin-1), e.g. the UTF-8 bytes of `é` are `'Ã©'`.

The character set flags change the meaning of `\w`, `\d`, `\s`, `\b`, POSIX classes and case-insensitive matching in the same way as in the engine: for example, `a` (ASCII) and `u` (Unicode) of Python, Ruby and Perl, `U` (`UNICODE_CHARACTER_CLASS`) and `u` (`UNICODE_CASE`) of Java, and PCRE's `(?aD)` and the like.

### Perl

Perl has two more parameters.

- [`perlSource`](parameters.mdx#perlsource) tells how the pattern is given.
  `literal` (default) is a regular expression literal written in Perl source (e.g. `qr/.../`), where Perl applies `\U`, `\L`, `\u`, `\l`, `\F`, `\Q` and `\E` to the text before the regex engine reads it.
  `string` is a pattern from a string at runtime (e.g. `qr/$p/`), which the regex engine reads unchanged.
- [`perlUtf8`](parameters.mdx#perlutf8) tells whether the pattern string is UTF-8, which makes the `/d` rules (the default) the Unicode rules.
  `null` (default) decides it from the pattern.

## Approximations

`recheck` models the matching of a naive backtracking engine.
Most engines have optimizations that make some vulnerable patterns fast, and `recheck` does not model them.
For example, Ruby's memoization (since Ruby 3.2), Perl's caches of the super-linear matching, Java's optimizations of loops, and PCRE2's auto-possessification and JIT.

The following is what we measured with `^(a|a)*$` against `'a'.repeat(n) + '!'`.

- V8 (Node.js 26) and Python 3.15 take exponential time: about 2 seconds for `n = 24`.
- Ruby 4.0, Java 25, .NET 10 and Perl 5.44 finish in less than 1 ms even for `n = 28`.
- PHP 8.5 (PCRE2 10.49 with the default `pcre.backtrack_limit`) gives up with the backtrack limit error for `n = 20`.

So `recheck` reports `^(a|a)*$` as vulnerable in every dialect, but the attack is not effective on some engines.
[Recall validation](#recall-validation) runs the attack string on the real engine, and it tells whether the attack works there.

There are other deliberate approximations.

- Case-insensitive matching uses each dialect's mapping between single characters (e.g. JavaScript's `Canonicalize`, .NET's `ToLowerInvariant`, and the simple case folding of Ruby, Perl and PCRE).
  Ruby and Perl also match a character against several characters (e.g. `ß` against `ss`), but `recheck` handles it only for a run of plain literals.
  It does not handle a character class, a group or a quantifier taking part in such a match (e.g. `[s]s`).
- The Unicode data (properties, case mappings) is of Unicode 18.0, while the engines use Unicode 16 or 17.
- For JavaScript's `v` flag, `recheck` follows the specification.
- Look-arounds, back-references, atomic groups and possessive quantifiers are handled by the fuzz checker, not by the automaton checker.
- A back-reference sees the captures as the engine keeps them over the iterations of a loop: JavaScript resets them on each iteration, and Perl unsets the captures of a failed alternative and of a fixed-length repetition matching nothing (e.g. `^(?:(a)?b){2}\1$` does not match `abba` in Perl).
  Two behaviors are not reproduced: Java does not restore the captures in a repeated group without branches (e.g. `(?:(b)c)*`) when the repetition backtracks, even at a later starting position, and the engines differ on an iteration matching the empty string.

## Recall Validation

The recall validation runs the attack string on the real engine of the dialect.
It runs when [`recallTimeout`](parameters.mdx#recalltimeout) is positive, and only where `recheck` can run commands: it does not run in the browser or with the Scala.js build (the `worker` and `pure` backends of the JavaScript library).

```bash
recheck --dialect=ruby --recall-timeout=1s '/^(a|a)*$/'
```

```tsx
import { check } from "recheck";

await check("^(a|a)*$", "", { dialect: "ruby", recallTimeout: 1000 });
```

For each attack string, `recheck` generates a small program in the dialect's language, and runs it with the command in the table below.
The program is given on the standard input.
The timeout counts from the start of the matching, so the startup of the runtime (e.g. compiling the program) is not counted.

| `dialect`    | Command                       | Matching                              | Engine errors                                                    |
| ------------ | ----------------------------- | ------------------------------------- | ---------------------------------------------------------------- |
| `javascript` | `node -`                      | `RegExp.prototype.exec`               | `RangeError`                                                     |
| `python`     | `python3 -I -`                | `re.search`                           | `RecursionError`, `MemoryError`                                  |
| `ruby`       | `ruby -`                      | `Regexp#match?` (no `Regexp.timeout`) | `RegexpError`, `NoMemoryError`, `SystemStackError`               |
| `java`       | `java --source 17 /dev/stdin` | `Matcher#find`                        | `StackOverflowError`, `OutOfMemoryError`                         |
| `dotnet`     | `dotnet run -`                | `Regex.Match`                         | `OutOfMemoryException`, `InsufficientExecutionStackException`    |
| `perl`       | `perl -`                      | `=~`                                  | none                                                             |
| `pcre`       | `php`                         | `preg_match`                          | the backtrack limit, the recursion limit and the JIT stack limit |

When the engine gives up with one of the engine errors, the attack is considered effective, so the recall validation succeeds as with a timeout.
The result records it as `engine-error` (see [Diagnostics](diagnostics.md#recall-validation)).

Notes on each dialect:

- `java` runs the program as a single-file source program, which needs a JDK (17 or later).
  It reads the program from `/dev/stdin`, so it does not work on Windows.
- `dotnet` needs the .NET 10 SDK for `dotnet run -`.
  It compiles each program on the first run, and caches it.
- `perl` puts the pattern into the program as a `qr` literal with the `literal` [`perlSource`](parameters.mdx#perlsource).
  When the literal would interpolate a variable or run code (e.g. `$name`, `@{...}` or `(?{ ... })`), the recall validation is skipped and `recallValidation` is `null`.
  With the `string` source, the program builds the pattern from a string as `qr/$pattern/`.
- `pcre` runs with PHP's default settings of `pcre.backtrack_limit`, `pcre.recursion_limit` and `pcre.jit`, and the runtime name in the result shows them.
  With the default limits, an exponential match usually ends with the backtrack limit error.

When the command cannot run (e.g. the runtime is not installed), the result is `unknown` with an `unexpected` error.

### Runtime Command

By default, the command is found by its name in `PATH`.
The [`recallCommand`](parameters.mdx#recallcommand) parameter replaces the runtime program with another command, given as the program and its arguments.
`recheck` appends the dialect's own arguments to read the program from the standard input (e.g. `-` for `ruby`, `-I -` for `python3`, and `run -` for `dotnet`).

```tsx
import { check } from "recheck";

// Runs `/opt/ruby-4.0/bin/ruby -`.
await check("^(a|a)*$", "", {
  dialect: "ruby",
  recallTimeout: 1000,
  recallCommand: ["/opt/ruby-4.0/bin/ruby"],
});

// Runs `docker run -i --rm ruby:4.0 ruby -`.
await check("^(a|a)*$", "", {
  dialect: "ruby",
  recallTimeout: 1000,
  recallCommand: ["docker", "run", "-i", "--rm", "ruby:4.0", "ruby"],
});
```

The CLI takes the command as one string separated by spaces.

```bash
recheck --dialect=ruby --recall-timeout=1s --recall-command='/opt/ruby-4.0/bin/ruby --disable-gems' '/^(a|a)*$/'
```

### Failed Recall Validation

When the recall validation fails, `check` does not report the diagnostics, and it goes on to look for another attack string.
If no attack succeeds, the result is `safe`.
`checkAll` returns the diagnostics whose recall validation failed too, followed by the reported one.
This shows that the pattern is vulnerable on a naive backtracking engine, though the dialect's engine is not affected by the found attacks.
