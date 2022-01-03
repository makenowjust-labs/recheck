# dev

Changes:

- Split `common` package into another module `recheck-common` ([#202](https://github.com/MakeNowJust-Labo/recheck/pull/202))
- Increase default parameter values ([#202](https://github.com/MakeNowJust-Labo/recheck/pull/202))
- Restrict repeats contained in gene ([#202](https://github.com/MakeNowJust-Labo/recheck/pull/202))
- Add `Parameters` as immutable version of `Config` ([#202](https://github.com/MakeNowJust-Labo/recheck/pull/202))
- Add `accelerationMode` parameter to specify acceleration mode behavior ([#211](https://github.com/MakeNowJust-Labo/recheck/pull/211))
- Rename default parameter constant names ([#211](https://github.com/MakeNowJust-Labo/recheck/pull/211))
- Add the new default seeder named `StaticSeeder` ([#227](https://github.com/MakeNowJust-Labo/recheck/pull/227))
- Decrease some parameters default values ([#227](https://github.com/MakeNowJust-Labo/recheck/pull/227))
- Improve repetition count estimation ([#227](https://github.com/MakeNowJust-Labo/recheck/pull/227))
- Use JavaScript style representation for `AttackPattern#toString` ([#229](https://github.com/MakeNowJust-Labo/recheck/pull/229))
- Apply look-ahead optimization to VM ([#233](https://github.com/MakeNowJust-Labo/recheck/pull/233))
- Add recall validation ([#234](https://github.com/MakeNowJust-Labo/recheck/pull/234))
- Simplify automaton without input terminator distinction ([#240](https://github.com/MakeNowJust-Labo/recheck/pull/240))
- Add `recheck-jar` package ([#248](https://github.com/MakeNowJust-Labo/recheck/pull/248))

Fixes:

- Fix semaphore usage in test ([#230](https://github.com/MakeNowJust-Labo/recheck/pull/230))
- Fix `sbt` file issues ([#231](https://github.com/MakeNowJust-Labo/recheck/pull/231))
- Check integer overflow on matching steps ([#232](https://github.com/MakeNowJust-Labo/recheck/pull/232))
- Fix loop detection in `StaticSeeder` ([#235](https://github.com/MakeNowJust-Labo/recheck/pull/235))
- Returns iterator in `FuzzChecker`  ([#236](https://github.com/MakeNowJust-Labo/recheck/pull/236))
- Rewrite `recheck` package implementation ([#248](https://github.com/MakeNowJust-Labo/recheck/pull/248))
- Add `recheck` package test ([#248](https://github.com/MakeNowJust-Labo/recheck/pull/248))

# 4.1.1 (2021-12-04)

Fixes:

- Add `index.d.ts` file to the package ([#184](https://github.com/MakeNowJust-Labo/recheck/pull/184))
- Fix `optionalDependencies` in `recheck` package ([#185](https://github.com/MakeNowJust-Labo/recheck/pull/185))

# 4.1.0 (2021-12-04)

Changes:

- Add `checkSync` function to NPM package ([#156](https://github.com/MakeNowJust-Labo/recheck/pull/156))
- Add `eslint-plugin-redos` package ([#160](https://github.com/MakeNowJust-Labo/recheck/pull/160))

Fixes:

- Set default value to check config ([#143](https://github.com/MakeNowJust-Labo/recheck/issues/143))
- Rewrite `recheck` package in TypeScript ([#149](https://github.com/MakeNowJust-Labo/recheck/pull/149))
- Call `subprocess.ref()`/`unref()` correctly ([#181](https://github.com/MakeNowJust-Labo/recheck/pull/181))

# 4.0.2 (2021-11-23)

A test release. (Second)

# 4.0.1 (2021-11-23)

A test release.

# 4.0.0 (2021-11-23)

Changes:

- Move unicode related types into new `recheck-unicode` module ([#77](https://github.com/MakeNowJust-Labo/recheck/pull/77))
- Move `RegExp` parser into new `recheck-parse` module ([#78](https://github.com/MakeNowJust-Labo/recheck/pull/78))
- Move JS binding into new `recheck-js` module ([#79](https://github.com/MakeNowJust-Labo/recheck/pull/79))
- Add `recheck` CLI ([#83](https://github.com/MakeNowJust-Labo/recheck/pull/83))
- Build `recheck` CLI by using Graal `native-image`
- Move circe codes into `recheck-codec` module ([#91](https://github.com/MakeNowJust-Labo/recheck/pull/91))
- Rename `recheck` module to `recheck-core` ([#92](https://github.com/MakeNowJust-Labo/recheck/pull/92))
- Add `ErrorKind.Cancel` for canceled execution ([#93](https://github.com/MakeNowJust-Labo/recheck/pull/93))
- Rename `EpsNFABuilder.compile` to `.build` ([#94](https://github.com/MakeNowJust-Labo/recheck/pull/94))
- Improve AST node structure ([#138](https://github.com/MakeNowJust-Labo/recheck/pull/138))
- Rename `batch` subcommand to `agent` ([#139](https://github.com/MakeNowJust-Labo/recheck/pull/139))
- Add `recheck-${os}-${cpu}` packages to distribute binaries ([#141](https://github.com/MakeNowJust-Labo/recheck/pull/139))
- Rename `@makenowjust-labo/recheck` package to `recheck` ([#141](https://github.com/MakeNowJust-Labo/recheck/pull/139))

Fixes:

- Update `sourcecode` to 0.2.7 ([#72](https://github.com/MakeNowJust-Labo/recheck/pull/72))
- Update Scala version to 2.13.6
- Run `recheck batch` requests in asynchronous ([#89](https://github.com/MakeNowJust-Labo/recheck/pull/89))
- Update Scala version tp 2.13.7

# 3.1.0 (2021-05-14)

Changes:

- Remove `isLineTerminator` and `isWord` flags from `IChar`
- Simplify `UString` implementation as `String` wrapper
- Send coverage to Codecov
- Rename `EpsNFACompiler` to `EpsNFABuilder`

Fixes:

- Fix nested look-around assertion behavior correctly
- Improve code coverage
- Refactor `EpsNFABuilder` by using a class.

# 3.0.0 (2021-04-14)

Changes:

- Add a new VM implementation for fuzzing :tada:
  - acceleration mode & opt-in analysis

# 2.1.0 (2021-02-08)

Changes:

- Add `loc` to RegExp AST nodes
- Add `hotspot` information to `Diagnostics`

# 2.0.2 (2021-02-04)

Fixes:

- Update the website
- Add ES module build to JS package

# 2.0.1 (2021-02-01)

Fixes:

- Upload JavaScript file correctly

# 2.0.0 (2021-02-01)

Changes:

- Use consuming steps instead of VM execution steps in fuzzing
- Add cancellable `Context`
- Refactor `Diagnostics`
- Rename the project name `redos` to `recheck`

# 1.2.0 (2021-01-02)

Changes:

- Drop `max` from `FString.Repeat`
- Rename the module name `redos-core` to `redos`
- Update Scala to 2.13.4
- Rename `MultiNFA` to `NFAwLA`

Fixes:

- Fix ε-loop elimination

# 1.1.2 (2020-12-04)

Fixes:

- Insert timeout checks more and more
- Improve some tests

# 1.1.1 (2020-11-24)

Changes:

- Add `used` checker information to `Diagnostics`

Fixes:

- Update default parameters
- Optimize the automaton based checker implementation

# 1.1.0 (2020-11-20)

Changes:

- Add `maxDegree` parameter

Fixes:

- Optimize VM implementation
- Improve attack string construction strategy

# 1.0.1 (2020-11-19)

Fixes:

- Handle timeout error in `ReDoS.check` correctly.

# 1.0.0 (2020-11-19)

Changes:

- Move `Compiler` to `automaton` package.
- Move `unicode` package to `data` package.
- Add backtrack based RegExp VM.
- Add fuzz checker.
- Add hybrid checker.

Fixes:

- Fix conversion from repeat pattern `a{n,m}` to ε-NFA correctly.
- Remove duplicated canonicalization of word escape class on `ignore-case`.

# 0.2.0 (2020-11-01)

Changes:

- Move `Checker`, `Complexity` and `Witness` into `automaton` package.
- Add `Diagnostics`.
- Use `Vector` instead for performance.
- Improve `package.json` contents.

Fixes:

- Refactor `EpsNFA.Assertion#accept`.

# 0.1.1 (2020-10-27)

Changes:

- Fix ε-elimination behavior with an assertion.
- Fix `IChar.Word` range.
- Add TypeScript definition file to NPM package.

# 0.1.0 (2020-10-27)

An initial release.
