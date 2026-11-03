---
id: runtime-attack-verification
title: Runtime-Attack Verification
---

Describes the runtime-attack verification, which runs the attack string on the real engine of the [dialect](dialects.md).

## Overview

`recheck` models a naive backtracking engine, and some engines run a found attack fast by their optimizations (see [Approximations](dialects.md#approximations)).
The runtime-attack verification runs the attack string on the real engine of the dialect (the runtime attack), and it tells whether the attack works there.
It runs when [`runtimeAttackTimeout`](parameters.mdx#runtimeattacktimeout) is positive, and only where `recheck` can run commands: it does not run in the browser or with the Scala.js build (the `worker` and `pure` backends of the JavaScript library).

```bash
recheck --dialect=ruby --runtime-attack-timeout=1s '/^(a|a)*$/'
```

```tsx
import { check } from "recheck";

await check("^(a|a)*$", "", { dialect: "ruby", runtimeAttackTimeout: 1000 });
```

The result is recorded as `runtimeAttack` of the diagnostics, or in `refutedAttacks` when the attack is refuted (see [Diagnostics](diagnostics.md#runtime-attack-verification)).

## Programs

For each attack string, `recheck` generates a small program in the dialect's language, and runs it with the command in the table below.
The program is given on the standard input, except for `java` and `dotnet` (see [Program Files](#program-files)).
The timeout counts from the start of the matching, so the startup of the runtime (e.g. compiling the program) is not counted.
The program sets the locale that `recheck` assumes before compiling the pattern, so the result does not depend on the locale of the environment (e.g. `LC_ALL` or `LANG`): the C locale for `perl` and `python`, and the invariant culture for `dotnet`.

| `dialect`                                                          | Command                                                   | Matching                              | Engine limits       | Engine errors                                                 |
| ------------------------------------------------------------------ | --------------------------------------------------------- | ------------------------------------- | ------------------- | ------------------------------------------------------------- |
| [`javascript`](dialects/javascript.md#runtime-attack-verification) | `node -`                                                  | `RegExp.prototype.exec`               | none                | `RangeError`                                                  |
| [`python`](dialects/python.md#runtime-attack-verification)         | `python3 -I -`                                            | `re.search`                           | none                | `RecursionError`, `MemoryError`                               |
| [`ruby`](dialects/ruby.md#runtime-attack-verification)             | `ruby -`                                                  | `Regexp#match?` (no `Regexp.timeout`) | none                | `RegexpError`, `NoMemoryError`, `SystemStackError`            |
| [`java`](dialects/java.md#runtime-attack-verification)             | `java --source 17 <dir>/RecheckRuntimeAttack-<hash>.java` | `Matcher#find`                        | none                | `StackOverflowError`, `OutOfMemoryError`                      |
| [`dotnet`](dialects/dotnet.md#runtime-attack-verification)         | `dotnet run <dir>/RecheckRuntimeAttack-<hash>.cs`         | `Regex.Match`                         | none                | `OutOfMemoryException`, `InsufficientExecutionStackException` |
| [`perl`](dialects/perl.md#runtime-attack-verification)             | `perl -`                                                  | `=~`                                  | none                | none                                                          |
| [`pcre`](dialects/pcre.md#runtime-attack-verification)             | `php`                                                     | `preg_match`                          | the backtrack limit | the recursion limit and the JIT stack limit                   |

Each dialect's page has the notes on its program (e.g. the requirements of `java` and `dotnet`, and when the runtime-attack verification of `perl` is skipped).

An engine limit is a limit on the number of the matching steps.
When the engine stops on it, the attack is considered effective, so the runtime-attack verification confirms the attack as with a timeout.
The result records it as `engine-limit`.

An engine error is an error on the stack depth or the memory.
The engines may raise it on a long string even if the matching takes linear time (e.g. V8 raises `RangeError` for `/^(?:a|b)*$/` on 10,000,000 characters), so it does not show that the attack is effective, and the runtime-attack verification refutes the attack.
The result records it as `engine-error` with the message (see [Diagnostics](diagnostics.md#runtime-attack-verification)).

When the command cannot run (e.g. the runtime is not installed), the result is `unknown` with an `unexpected` error.

### Program Files

`java` and `dotnet` run a fixed program file, which reads the pattern and the attack string from the standard input.
`recheck` writes the programs of `java` and `dotnet` once into the directory `recheck-<user>` in the temporary directory of the JVM (`java.io.tmpdir`), and it keeps them there.
The directory must be owned only by the user (`rwx------`).
If it is not, or the file system has no POSIX permissions (e.g. on Windows), `recheck` uses a new temporary directory, which it deletes on exit.

## Runtime Command

By default, the command is found by its name in `PATH`.
An environment variable of each dialect replaces the runtime program with a command line.

| `dialect`    | Environment variable |
| ------------ | -------------------- |
| `javascript` | `RECHECK_NODE`       |
| `python`     | `RECHECK_PYTHON`     |
| `ruby`       | `RECHECK_RUBY`       |
| `java`       | `RECHECK_JAVA`       |
| `dotnet`     | `RECHECK_DOTNET`     |
| `perl`       | `RECHECK_PERL`       |
| `pcre`       | `RECHECK_PHP`        |

The value is a command line run by `/bin/sh -c`, and `recheck` appends the dialect's own arguments to it (e.g. `-` for `ruby`, `-I -` for `python3`, and `run` and the program file for `dotnet`).
So the shell splits the value on spaces, and a path with spaces needs quotes.
An empty value is ignored.

```bash
# Runs `/opt/ruby-4.0/bin/ruby --disable-gems -`.
RECHECK_RUBY='/opt/ruby-4.0/bin/ruby --disable-gems' recheck --dialect=ruby --runtime-attack-timeout=1s '/^(a|a)*$/'

# A path with spaces.
RECHECK_RUBY='"/Applications/My Ruby/bin/ruby" --disable-gems' recheck --dialect=ruby --runtime-attack-timeout=1s '/^(a|a)*$/'

# Runs Ruby in a container, which reads the program from the standard input.
RECHECK_RUBY='docker run -i --rm ruby:4.0 ruby' recheck --dialect=ruby --runtime-attack-timeout=1s '/^(a|a)*$/'
```

The JavaScript library and the ESLint plugin pass their environment to the `recheck` process, so the variables work there too.
The command of `java` and `dotnet` must be able to read the program file in the temporary directory, so a container needs the directory mounted at the same path.
On Windows, the variables need `sh` in `PATH` (e.g. from Git for Windows), and `recheck` has not been tested there.

## Refuted Attacks

When the runtime-attack verification refutes an attack, `check` does not report the diagnostics, and it goes on to look for another attack string.
If no attack is confirmed, the result is `safe`.
The result lists the refuted attacks in its `refutedAttacks` field (see [Diagnostics](diagnostics.md#runtime-attack-verification)).
They show that the pattern is vulnerable on a naive backtracking engine, though the dialect's engine is not affected by these attacks.
The CLI shows them in the `Refuted attacks` section after the result.

```bash
recheck --dialect=ruby --runtime-attack-timeout=1s '/^(a|a)*$/'
```

```
Input     : /^(a|a)*$/
Dialect   : ruby (encoding: UTF-8)
Status    : safe
Complexity: linear
Checker   : automaton

Refuted attacks:
- Complexity    : exponential
  Attack string : "a" + "a" * 31 + "\x00"
  Runtime attack: refuted (finished in 0 ms on ruby 4.0.7, timeout: 1000 ms)
```

The search avoids repeating a refuted attack.

- An attack with the same complexity (the type and the degree) and the same hotspot (the heat spots) as a refuted one is skipped without the runtime attack, and it is not in `refutedAttacks`.
- The fuzz checker gives the strings whose attacks were refuted a lower priority in the next generation, so that it looks into other structures of the pattern.
- When the runtime-attack verification refutes [`maxRefutedAttacks`](parameters.mdx#maxrefutedattacks) attacks (10 by default), the checking stops, and the result is `unknown` with the `runtime-attacks-refuted` error.

## Runtime-Attack Script

The CLI's `generate-runtime-attack-script` subcommand writes the program that the runtime-attack verification runs as a standalone script, so that you can run the attack on the engine yourself.
It takes a file of `vulnerable` diagnostics in JSON (`-` reads the standard input), which is the output of the CLI with `--json`, or the result of the JavaScript library or the CLI's agent mode.
The dialect of the pattern comes from the `dialect` field of the diagnostics, and the unit of the attack string from the `unitMode` field.

```bash
recheck --json --dialect=python '/^(a|a)*$/' > diagnostics.json
recheck generate-runtime-attack-script diagnostics.json > runtime-attack.py
python3 -I runtime-attack.py
```

The first comment of the script shows the command to run it, which is the command of the runtime attack with the script file.
The script is the program of the runtime-attack verification, so it sets the locale in the same way.
The script of `java` and `dotnet` has the pattern and the attack string in itself.

| `dialect`    | Command                                |
| ------------ | -------------------------------------- |
| `javascript` | `node runtime-attack.cjs`              |
| `python`     | `python3 -I runtime-attack.py`         |
| `ruby`       | `ruby runtime-attack.rb`               |
| `java`       | `java --source 17 runtime-attack.java` |
| `dotnet`     | `dotnet run runtime-attack.cs`         |
| `perl`       | `perl runtime-attack.pl`               |
| `pcre`       | `php runtime-attack.php`               |

The script prints the lines that the runtime-attack verification reads: `recheck:runtime` with the runtime, `recheck:start` before the matching, and `recheck:time` with the matching time in milliseconds (or `recheck:engine-limit` or `recheck:engine-error` with the message).
It has no timeout, so an effective attack does not end soon.

When the diagnostics are not `vulnerable`, or the pattern cannot be put in the program (the case where the runtime-attack verification is skipped for [`perl`](dialects/perl.md#runtime-attack-verification)), the subcommand fails with a message.
