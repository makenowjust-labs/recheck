---
id: dialects
title: Dialects
---

Describes the regular expression dialects that `recheck` supports, and what is common to all of them.
Each dialect has its own page on how it is checked.

## Choosing a Dialect

`recheck` checks a regular expression in the syntax and the semantics of one dialect.
The [`dialect`](parameters.mdx#dialect) parameter chooses it, and its default is `javascript`.
It is the name of the dialect, or an object of the name and the dialect's settings (e.g. `{ "name": "python", "type": "bytes" }`), where an omitted setting takes the default.
The flags are the dialect's own flag letters (e.g. `imsx`), not a JavaScript flags string.

The following shows how to check a pattern in the Python dialect.

```bash
recheck --dialect=python '/^(a|a)*$/'
recheck --dialect=python --python-type=bytes '/^(a|a)*$/'
```

```tsx
import { check } from "recheck";

await check("^(a|a)*$", "", { dialect: "python" });
await check("^(a|a)*$", "", { dialect: { name: "python", type: "bytes" } });
```

```scala
import codes.quine.labs.recheck.ReDoS
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.resyntax.ast.Dialect

ReDoS.check("^(a|a)*$", "", Parameters(dialect = Dialect.Python()))
ReDoS.check("^(a|a)*$", "", Parameters(dialect = Dialect.Python(Dialect.Python.Type.Bytes)))
```

The CLI takes the settings by `--python-type`, `--ruby-encoding`, `--perl-utf8` and `--perl-source`, and an option of another dialect is an error.
The Playground also has a selector of the dialect, and it shows the settings of the selected dialect.

:::note

The ESLint plugin (`eslint-plugin-redos`) checks JavaScript regular expressions only, so it has no `dialect` option.

:::

## Supported Dialects

| `dialect`                              | Engine that `recheck` follows                  | Matching unit                                      | Recall validation command                          |
| -------------------------------------- | ---------------------------------------------- | -------------------------------------------------- | -------------------------------------------------- |
| [`javascript`](dialects/javascript.md) | ECMAScript 2025 `RegExp` (V8)                  | UTF-16 code units, or code points with `u` or `v`  | `node -`                                           |
| [`python`](dialects/python.md)         | Python 3.15 `re`                               | code points (`str`), or bytes (`bytes`)            | `python3 -I -`                                     |
| [`ruby`](dialects/ruby.md)             | Ruby 4.0 `Regexp` (Onigmo)                     | code points (`UTF-8`), or bytes (`ASCII-8BIT`)     | `ruby -`                                           |
| [`java`](dialects/java.md)             | Java 25 `java.util.regex.Pattern`              | code points                                        | `java --source 17 <dir>/RecheckRecall-<hash>.java` |
| [`dotnet`](dialects/dotnet.md)         | .NET 10 `System.Text.RegularExpressions.Regex` | UTF-16 code units                                  | `dotnet run <dir>/RecheckRecall-<hash>.cs`         |
| [`perl`](dialects/perl.md)             | Perl 5.44 regular expressions                  | code points (`utf8`), or bytes (no `utf8`)         | `perl -`                                           |
| [`pcre`](dialects/pcre.md)             | PCRE2 10.49 as PHP's `preg_*` functions use it | bytes, or code points (UTF-8) with `u` or `(*UTF)` | `php`                                              |

Each dialect's page describes its engine, settings, flags and units, syntax limitations, approximations and recall validation.
The rest of this page describes what is common to all the dialects.

## Units and Attack Strings

A pattern matches a string of UTF-16 code units, of code points, or of bytes, which depends on the dialect.
The dialects having the choice take a setting of the [`dialect`](parameters.mdx#dialect) in the dialect's own words: `type` of `python`, `encoding` of `ruby` and `utf8` of `perl`.
The setting also changes how the pattern is read, as the engine does.

The attack string (`attack.pattern` of the diagnostics) is an expression in the dialect's host language, e.g. `'a' * 31 + '\x00'` in Python (see [Diagnostics](diagnostics.md#attack-string) for each dialect).
In a bytes mode, it is a bytes literal (`b'\xc3\xa9'` in Python, `"\xC3\xA9".b` in Ruby, `"\xC3\xA9"` in Perl and PHP).
`attack.string` shows each byte as the character of the same code (as Latin-1), e.g. the UTF-8 bytes of `é` are `Ã©`.

The character set flags change the meaning of `\w`, `\d`, `\s`, `\b`, POSIX classes and case-insensitive matching in the same way as in the engine.

## Syntax Limitations

`recheck` reads the whole syntax of the latest version of each engine in the table above.
It also reproduces, as far as possible, the errors that the engine reports after reading the syntax.
A pattern that the engine rejects is reported as `unknown` with an `invalid` error.

Some constructs are recognized, but the checkers do not handle them.
A pattern having one of them is reported as `unknown` with an `unsupported` error naming the construct (e.g. `a(?R)?` gives `unsupported syntax (RCall) at 1`).
The following are not supported in any dialect having them, and each dialect's page lists its own ones (e.g. .NET's balancing groups).

- Recursion and subroutine calls (e.g. `(?R)`, `(?1)`, `(?&name)`, `\g<name>`), and the conditionals depending on them (`(?(R)...)`, and `(?(DEFINE)...)` in a pattern with calls).
- Callouts (e.g. `(?C1)`) and embedded code (e.g. Perl's `(?{ ... })` and `(??{ ... })`).
- `\N{name}` and `\C` in a mode other than bytes.
- Word boundaries of a kind, such as `\b{wb}`.
- Nesting deeper than 150 levels, where each group, look-around condition of a conditional, class, operand of a class set operation, and repeat of a repeat is one level (e.g. `(?:` repeated 151 times gives `Too deeply nested (limit 150) at 450`).

## Approximations

`recheck` models the matching of a naive backtracking engine.
Most engines have optimizations that make some vulnerable patterns fast, and `recheck` does not model them.
Examples are Ruby's memoization (since Ruby 3.2), Perl's caches of the super-linear matching, Java's optimizations of loops, and PCRE2's auto-possessification and JIT.

The following is what we measured with `^(a|a)*$` against `'a'.repeat(n) + '!'`.

- V8 (Node.js 26) and Python 3.15 take exponential time: about 2 seconds for `n = 24`.
- Ruby 4.0, Java 25, .NET 10 and Perl 5.44 finish in less than 1 ms even for `n = 28`.
- PHP 8.5 (PCRE2 10.49 with the default `pcre.backtrack_limit`) gives up with the backtrack limit error for `n = 20`.

So `recheck` reports `^(a|a)*$` as vulnerable in every dialect, but the attack is not effective on some engines.
[Recall Validation](recall-validation.md) runs the attack string on the real engine, and it tells whether the attack works there.

The following approximations are common to the dialects, and each dialect's page describes its own ones.

- The Unicode data (properties, case mappings) is of Unicode 18.0, while the engines use Unicode 16 or 17.
- Look-arounds (including non-atomic ones), back-references, atomic groups, possessive quantifiers, conditionals and the backtracking control verbs other than `(*FAIL)` and `(*MARK)` are handled by the fuzz checker, not by the automaton checker.

### Searches

`recheck` models one search from the start of the input, which retries at each following position (unless the pattern is anchored).

- `\G` matches at the start of the search, so it is the start of the input there, as JavaScript's `y` flag is.
  A search from a later position (e.g. the second match of a global search, Java's `find(int)`, Perl's `pos()` or Python's `search(s, pos)`) is not modeled.
  So a pattern can be slow only on such a search, where a look-behind, `\b`, `\B` or a multiline `^` sees the text before the starting position (e.g. `(?<=a)\G(?:b|b)*$` on `'a' + 'b'.repeat(n) + '!'` from the position `1`), and `recheck` misses it.

## Recall Validation

The [recall validation](recall-validation.md) runs the attack string on the real engine of the dialect, and it tells whether the attack works there.
Each dialect's page has the command of the recall validation and the notes on its program.
