# dev

New Features:

- [#1636](https://github.com/makenowjust-labs/recheck/pull/1636) Add a Linux ARM64 build ([@makenowjust](https://github.com/makenowjust))

Fixes:

- [#1615](https://github.com/makenowjust-labs/recheck/pull/1615) Support ESLint flat config ([@makenowjust](https://github.com/makenowjust))
- [#1620](https://github.com/makenowjust-labs/recheck/pull/1620) Fix #1616: resolve the windows path issue ([@makenowjust](https://github.com/makenowjust))
- [#1631](https://github.com/makenowjust-labs/recheck/pull/1631) Switch to pnpm ([@makenowjust](https://github.com/makenowjust))
- [#1638](https://github.com/makenowjust-labs/recheck/pull/1638) Fix `ReDoS.checkAuto` test for preventing flaky test ([@makenowjust](https://github.com/makenowjust))

Misc:

- [#1583](https://github.com/makenowjust-labs/recheck/pull/1583) Update circe-generic to 0.14.15 ([@makenowjust](https://github.com/makenowjust))
- [#1577](https://github.com/makenowjust-labs/recheck/pull/1577) Update circe-scalajs to 0.14.15 ([@makenowjust](https://github.com/makenowjust))
- [#1576](https://github.com/makenowjust-labs/recheck/pull/1576) Update circe-core, circe-generic, ... to 0.14.15 ([@makenowjust](https://github.com/makenowjust))
- [#1582](https://github.com/makenowjust-labs/recheck/pull/1582) Update sbt-scalafix to 0.14.4 ([@makenowjust](https://github.com/makenowjust))
- [#1544](https://github.com/makenowjust-labs/recheck/pull/1544) Update sbt-ci-release to 1.11.2 ([@makenowjust](https://github.com/makenowjust))
- [#1501](https://github.com/makenowjust-labs/recheck/pull/1501) Update jest monorepo to v30 (major) ([@renovate[bot]](https://github.com/apps/renovate))
- [#1510](https://github.com/makenowjust-labs/recheck/pull/1510) Update dependency prettier to v3.6.2 ([@renovate[bot]](https://github.com/apps/renovate))
- [#1622](https://github.com/makenowjust-labs/recheck/pull/1622) Update GH action timeout to 15 min ([@makenowjust](https://github.com/makenowjust))
- [#1625](https://github.com/makenowjust-labs/recheck/pull/1625) Update timeout and runners to build ([@makenowjust](https://github.com/makenowjust))
- [#1626](https://github.com/makenowjust-labs/recheck/pull/1626) Run the `native-build` task on a `schedule` event ([@makenowjust](https://github.com/makenowjust))
- [#1627](https://github.com/makenowjust-labs/recheck/pull/1627) Add the `--detectOpenHandles` option to run test ([@makenowjust](https://github.com/makenowjust))
- [#1628](https://github.com/makenowjust-labs/recheck/pull/1628) Enable `--forceExit` option of Jest ([@makenowjust](https://github.com/makenowjust))
- [#1629](https://github.com/makenowjust-labs/recheck/pull/1629) Unref stderr and work test on CI env correctly ([@makenowjust](https://github.com/makenowjust))
- [#1637](https://github.com/makenowjust-labs/recheck/pull/1637) Use `lerna run test` instead of `pnpm test` ([@makenowjust](https://github.com/makenowjust))

# 4.5.0 (2025-03-02)

New Features:

- [#775](https://github.com/makenowjust-labs/recheck/pull/775) Add `cache` option to `eslint-plugin-redos` ([@makenowjust](https://github.com/makenowjust))
- [#799](https://github.com/makenowjust-labs/recheck/pull/799) Introduce `synckit` for speed-up `checkSync` ([@makenowjust](https://github.com/makenowjust))
- [#1179](https://github.com/makenowjust-labs/recheck/pull/1179) Add `recheck-macos-arm64` ([@makenowjust](https://github.com/makenowjust))

Fixes:

- [#777](https://github.com/makenowjust-labs/recheck/pull/777) Fix up cache implementation ([@makenowjust](https://github.com/makenowjust))
- [#800](https://github.com/makenowjust-labs/recheck/pull/800) Set `'conservative'` as default `cache.strategy` ([@makenowjust](https://github.com/makenowjust))
- [#805](https://github.com/makenowjust-labs/recheck/pull/805) Fix to decode `Duration` for `String` ([@makenowjust](https://github.com/makenowjust))
- [#872](https://github.com/makenowjust-labs/recheck/pull/872) Fix to work `RECHECK_SYNC_BACKEND` correctly ([@makenowjust](https://github.com/makenowjust))
- [#1175](https://github.com/makenowjust-labs/recheck/pull/1175) Update supported Node.js version ([@makenowjust](https://github.com/makenowjust))
- [#1402](https://github.com/makenowjust-labs/recheck/pull/1402) Use React v18 for website ([@makenowjust](https://github.com/makenowjust))
- [#1406](https://github.com/makenowjust-labs/recheck/pull/1406) Drop Node v18 support ([@makenowjust](https://github.com/makenowjust))

Misc:

- [#776](https://github.com/makenowjust-labs/recheck/pull/776) Fix heading in blog articles ([@makenowjust](https://github.com/makenowjust))
- [#781](https://github.com/makenowjust-labs/recheck/pull/781) Fix OS version of scala-steward CI for `GLIBC` version problem ([@makenowjust](https://github.com/makenowjust))
- [#780](https://github.com/makenowjust-labs/recheck/pull/780) Update dependency org.scalameta:scalafmt-core_2.13 to v3.7.2 ([@renovate[bot]](https://github.com/apps/renovate))
- [#873](https://github.com/makenowjust-labs/recheck/pull/873) Fix React version to v17 ([@makenowjust](https://github.com/makenowjust))
- [#874](https://github.com/makenowjust-labs/recheck/pull/874) Drop Node `v14` support, and add Node `v20` support ([@makenowjust](https://github.com/makenowjust))
- [#1140](https://github.com/makenowjust-labs/recheck/pull/1140) Update dependency synckit to v0.9.0 ([@renovate[bot]](https://github.com/apps/renovate))
- [#1084](https://github.com/makenowjust-labs/recheck/pull/1084) Update docusaurus monorepo to v3 (major) ([@renovate[bot]](https://github.com/apps/renovate))
- [#1177](https://github.com/makenowjust-labs/recheck/pull/1177) Update JVM to use on CI ([@makenowjust](https://github.com/makenowjust))
- [#1181](https://github.com/makenowjust-labs/recheck/pull/1181) Enable Google Analytics for website ([@makenowjust](https://github.com/makenowjust))
- [#1184](https://github.com/makenowjust-labs/recheck/pull/1184) Add `recheck-macos-arm64` to `recheck`'s `optionalDeps` ([@makenowjust](https://github.com/makenowjust))
- [#1185](https://github.com/makenowjust-labs/recheck/pull/1185) Add a test for `RECHECK_SYNC_BACKEND` env ([@makenowjust](https://github.com/makenowjust))
- [#1195](https://github.com/makenowjust-labs/recheck/pull/1195) Add `timeout-minutes` to workflow ([@makenowjust](https://github.com/makenowjust))
- [#1386](https://github.com/makenowjust-labs/recheck/pull/1386) Update Scala/Scala library/SBT plugin versions ([@makenowjust](https://github.com/makenowjust))
- [#1389](https://github.com/makenowjust-labs/recheck/pull/1389) Update JS deps ([@makenowjust](https://github.com/makenowjust))
- [#1390](https://github.com/makenowjust-labs/recheck/pull/1390) Update dependency @algolia/client-search to v5.20.3 ([@renovate[bot]](https://github.com/apps/renovate))
- [#1391](https://github.com/makenowjust-labs/recheck/pull/1391) Update dependency lerna to v8.2.0 ([@renovate[bot]](https://github.com/apps/renovate))
- [#1395](https://github.com/makenowjust-labs/recheck/pull/1395) Use ESLint v9 ([@makenowjust](https://github.com/makenowjust))
- [#1404](https://github.com/makenowjust-labs/recheck/pull/1404) Pin react-dom version ([@makenowjust](https://github.com/makenowjust))
- [#1405](https://github.com/makenowjust-labs/recheck/pull/1405) Update copyright year 2024 -> 2025 ([@makenowjust](https://github.com/makenowjust))

# 4.4.5 (2023-02-15)

Fixes:

- [#768](https://github.com/makenowjust-labs/recheck/pull/768) Show `stderr` output of agent processes ([@makenowjust](https://github.com/makenowjust))

Misc:

- [#767](https://github.com/makenowjust-labs/recheck/pull/767) Fix links in doc comments ([@makenowjust](https://github.com/makenowjust))

# 4.4.4 (2023-02-14)

Fixes:

- [#760](https://github.com/makenowjust-labs/recheck/pull/760) Pin the version of the CI environments for preventing GLIBC version warning ([@makenowjust](https://github.com/makenowjust))
- [#764](https://github.com/makenowjust-labs/recheck/pull/764) Remove deprecated `System.runFinalizer` call ([@makenowjust](https://github.com/makenowjust))

Misc:

- [#761](https://github.com/makenowjust-labs/recheck/pull/761) Use `$GITHUB_OUTPUT` instead of `set-output` ([@makenowjust](https://github.com/makenowjust))
- [#753](https://github.com/makenowjust-labs/recheck/pull/753) Update circe-core, circe-generic, ... to 0.14.4 ([@makenowjust](https://github.com/makenowjust))
- [#762](https://github.com/makenowjust-labs/recheck/pull/762) Rename artifact files before release ([@makenowjust](https://github.com/makenowjust))
- [#765](https://github.com/makenowjust-labs/recheck/pull/765) Remove useless `@nowarn` annotation ([@makenowjust](https://github.com/makenowjust))

# 4.4.3 (2023-01-23)

Fixes:

- [#720](https://github.com/makenowjust-labs/recheck/pull/720) Correctly fix `esbuild` config for Node v14 ([@makenowjust](https://github.com/makenowjust))

Misc:

- [#700](https://github.com/makenowjust-labs/recheck/pull/700) Update workflow for using coursier/setup-action ([@makenowjust](https://github.com/makenowjust))
- [#705](https://github.com/makenowjust-labs/recheck/pull/705) Update GraalVM version ([@makenowjust](https://github.com/makenowjust))
- [#711](https://github.com/makenowjust-labs/recheck/pull/711) Update copyright year 2022 -> 2023 ([@makenowjust](https://github.com/makenowjust))

# 4.4.2 (2023-01-05)

New Features:

- [#698](https://github.com/makenowjust-labs/recheck/pull/698) Add `recommended` config to the ESLint plugin ([@makenowjust](https://github.com/makenowjust))

Misc:

- [#501](https://github.com/makenowjust-labs/recheck/pull/501) Convert duration params to integer explicitly on playground ([@makenowjust](https://github.com/makenowjust))
- [#609](https://github.com/makenowjust-labs/recheck/pull/609) Update Node.js to v18 ([@makenowjust](https://github.com/makenowjust))
- [#699](https://github.com/makenowjust-labs/recheck/pull/699) Use `action-gh-release` ([@makenowjust](https://github.com/makenowjust))

# 4.4.1 (2022-06-17)

Fixes:

- [#491](https://github.com/makenowjust-labs/recheck/pull/491) Fix `Duration` and `Parameter` decoder for missing keys and `null` ([@makenowjust](https://github.com/makenowjust))

# 4.4.0 (2022-05-17)

Changes:

- [#357](https://github.com/makenowjust-labs/recheck/pull/357) Add `Default` prefix to default parameter values ([@makenowjust](https://github.com/makenowjust))
- [#358](https://github.com/makenowjust-labs/recheck/pull/358) Sort `Parameters` names in lexicographic order ([@makenowjust](https://github.com/makenowjust))
- [#448](https://github.com/makenowjust-labs/recheck/pull/448) Allow all parameters to ESLint plugin options ([@makenowjust](https://github.com/makenowjust))

Fixes:

- [#341](https://github.com/makenowjust-labs/recheck/pull/341) Fix zero-width assertion in `automaton` checker ([@makenowjust](https://github.com/makenowjust))
- [#380](https://github.com/makenowjust-labs/recheck/pull/380) Correct to increase steps on back-reference ([@makenowjust](https://github.com/makenowjust))
- [#411](https://github.com/makenowjust-labs/recheck/pull/411) Update Scala.js to 1.10.0 ([@makenowjust](https://github.com/makenowjust))

Misc:

- [#337](https://github.com/makenowjust-labs/recheck/pull/337) Increase timeout on `main.ts` test ([@makenowjust](https://github.com/makenowjust))
- [#343](https://github.com/makenowjust-labs/recheck/pull/343) Enable Algolia DocSearch ([@makenowjust](https://github.com/makenowjust))
- [#360](https://github.com/makenowjust-labs/recheck/pull/360) Enable log on playground ([@makenowjust](https://github.com/makenowjust))
- [#369](https://github.com/makenowjust-labs/recheck/pull/369) Fix Windows build ([@makenowjust](https://github.com/makenowjust))
- [#368](https://github.com/makenowjust-labs/recheck/pull/368) Fix log scroll ([@makenowjust](https://github.com/makenowjust))
- [#374](https://github.com/makenowjust-labs/recheck/pull/374) Format `package.json` by using https://devina.io/package-json-formatter ([@makenowjust](https://github.com/makenowjust))
- [#379](https://github.com/makenowjust-labs/recheck/pull/379) Fix `coreJVM/initialCommands` to run ([@makenowjust](https://github.com/makenowjust))

# 4.3.0 (2022-02-05)

New Features:

- [#289](https://github.com/makenowjust-labs/recheck/pull/289) Add `worker` backend to the NPM package ([@makenowjust](https://github.com/makenowjust))

Changes:

- [#283](https://github.com/makenowjust-labs/recheck/pull/283) Rename `hybrid` to `auto` ([@makenowjust](https://github.com/makenowjust))
- [#332](https://github.com/makenowjust-labs/recheck/pull/332) Rename `labo` to `labs` ([@makenowjust](https://github.com/makenowjust))

Fixes:

- [#284](https://github.com/makenowjust-labs/recheck/pull/284) Add missing field `AttackPattern#pattern` in TypeScript definition ([@makenowjust](https://github.com/makenowjust))

Misc:

- [#279](https://github.com/makenowjust-labs/recheck/pull/279) Update `CHANGELOG.md` automatically by GitHub Actions ([@makenowjust](https://github.com/makenowjust))
- [#280](https://github.com/makenowjust-labs/recheck/pull/280) Handle SBT deps by renovate correctly ([@makenowjust](https://github.com/makenowjust))
- [#281](https://github.com/makenowjust-labs/recheck/pull/281) Update scala-library, scala-reflect to 2.13.8 ([@makenowjust](https://github.com/makenowjust))
- [#282](https://github.com/makenowjust-labs/recheck/pull/282) Add note section to ESLint plugin document ([@makenowjust](https://github.com/makenowjust))
- [#286](https://github.com/makenowjust-labs/recheck/pull/286) Update `CHANGELOG.md` format ([@makenowjust](https://github.com/makenowjust))
- [#295](https://github.com/makenowjust-labs/recheck/pull/295) Renew document website powered by Docusaurus ([@makenowjust](https://github.com/makenowjust))
- [#313](https://github.com/makenowjust-labs/recheck/pull/313) Remove `mdoc` to build the document ([@makenowjust](https://github.com/makenowjust))
- [#314](https://github.com/makenowjust-labs/recheck/pull/314) Don't ignore to build website on CI test ([@makenowjust](https://github.com/makenowjust))
- [#318](https://github.com/makenowjust-labs/recheck/pull/318) Fix logo style ([@makenowjust](https://github.com/makenowjust))
- [#320](https://github.com/makenowjust-labs/recheck/pull/320) Improve documentation ([@makenowjust](https://github.com/makenowjust))

# 4.2.2 (2022-01-14)

Fixes:

- Fix `handleLine` in `Agent` for large data ([#266](https://github.com/makenowjust-labs/recheck/pull/266))
- Prefer to use `native` backend on `RECHECK_BACKEND=auto` ([#268](https://github.com/makenowjust-labs/recheck/pull/268))

# 4.2.1 (2022-01-08)

Fixes:

- Don't bundle recheck in eslint-plugin-redos ([#260](https://github.com/makenowjust-labs/recheck/pull/260))
- Use `require` directly instead of `module.require` ([#261](https://github.com/makenowjust-labs/recheck/pull/261))

# 4.2.0 (2022-01-08)

Changes:

- Split `common` package into another module `recheck-common` ([#202](https://github.com/makenowjust-labs/recheck/pull/202))
- Increase default parameter values ([#202](https://github.com/makenowjust-labs/recheck/pull/202))
- Restrict repeats contained in gene ([#202](https://github.com/makenowjust-labs/recheck/pull/202))
- Add `Parameters` as immutable version of `Config` ([#202](https://github.com/makenowjust-labs/recheck/pull/202))
- Add `accelerationMode` parameter to specify acceleration mode behavior ([#211](https://github.com/makenowjust-labs/recheck/pull/211))
- Rename default parameter constant names ([#211](https://github.com/makenowjust-labs/recheck/pull/211))
- Add the new default seeder named `StaticSeeder` ([#227](https://github.com/makenowjust-labs/recheck/pull/227))
- Decrease some parameters default values ([#227](https://github.com/makenowjust-labs/recheck/pull/227))
- Improve repetition count estimation ([#227](https://github.com/makenowjust-labs/recheck/pull/227))
- Use JavaScript style representation for `AttackPattern#toString` ([#229](https://github.com/makenowjust-labs/recheck/pull/229))
- Apply look-ahead optimization to VM ([#233](https://github.com/makenowjust-labs/recheck/pull/233))
- Add recall validation ([#234](https://github.com/makenowjust-labs/recheck/pull/234))
- Simplify automaton without input terminator distinction ([#240](https://github.com/makenowjust-labs/recheck/pull/240))
- Add `recheck-jar` package ([#248](https://github.com/makenowjust-labs/recheck/pull/248))
- Improve `StaticSeeding.simplify` against look-around near by repeat ([#255](https://github.com/makenowjust-labs/recheck/pull/255))

Fixes:

- Fix semaphore usage in test ([#230](https://github.com/makenowjust-labs/recheck/pull/230))
- Fix `sbt` file issues ([#231](https://github.com/makenowjust-labs/recheck/pull/231))
- Check integer overflow on matching steps ([#232](https://github.com/makenowjust-labs/recheck/pull/232))
- Fix loop detection in `StaticSeeder` ([#235](https://github.com/makenowjust-labs/recheck/pull/235))
- Returns iterator in `FuzzChecker`  ([#236](https://github.com/makenowjust-labs/recheck/pull/236))
- Rewrite `recheck` package implementation ([#248](https://github.com/makenowjust-labs/recheck/pull/248))
- Add `recheck` package test ([#248](https://github.com/makenowjust-labs/recheck/pull/248))
- Optimize alphabet construction ([#258](https://github.com/makenowjust-labs/recheck/pull/258))

# 4.1.1 (2021-12-04)

Fixes:

- Add `index.d.ts` file to the package ([#184](https://github.com/makenowjust-labs/recheck/pull/184))
- Fix `optionalDependencies` in `recheck` package ([#185](https://github.com/makenowjust-labs/recheck/pull/185))

# 4.1.0 (2021-12-04)

Changes:

- Add `checkSync` function to NPM package ([#156](https://github.com/makenowjust-labs/recheck/pull/156))
- Add `eslint-plugin-redos` package ([#160](https://github.com/makenowjust-labs/recheck/pull/160))

Fixes:

- Set default value to check config ([#143](https://github.com/makenowjust-labs/recheck/issues/143))
- Rewrite `recheck` package in TypeScript ([#149](https://github.com/makenowjust-labs/recheck/pull/149))
- Call `subprocess.ref()`/`unref()` correctly ([#181](https://github.com/makenowjust-labs/recheck/pull/181))

# 4.0.2 (2021-11-23)

A test release. (Second)

# 4.0.1 (2021-11-23)

A test release.

# 4.0.0 (2021-11-23)

Changes:

- Move unicode related types into new `recheck-unicode` module ([#77](https://github.com/makenowjust-labs/recheck/pull/77))
- Move `RegExp` parser into new `recheck-parse` module ([#78](https://github.com/makenowjust-labs/recheck/pull/78))
- Move JS binding into new `recheck-js` module ([#79](https://github.com/makenowjust-labs/recheck/pull/79))
- Add `recheck` CLI ([#83](https://github.com/makenowjust-labs/recheck/pull/83))
- Build `recheck` CLI by using Graal `native-image`
- Move circe codes into `recheck-codec` module ([#91](https://github.com/makenowjust-labs/recheck/pull/91))
- Rename `recheck` module to `recheck-core` ([#92](https://github.com/makenowjust-labs/recheck/pull/92))
- Add `ErrorKind.Cancel` for canceled execution ([#93](https://github.com/makenowjust-labs/recheck/pull/93))
- Rename `EpsNFABuilder.compile` to `.build` ([#94](https://github.com/makenowjust-labs/recheck/pull/94))
- Improve AST node structure ([#138](https://github.com/makenowjust-labs/recheck/pull/138))
- Rename `batch` subcommand to `agent` ([#139](https://github.com/makenowjust-labs/recheck/pull/139))
- Add `recheck-${os}-${cpu}` packages to distribute binaries ([#141](https://github.com/makenowjust-labs/recheck/pull/139))
- Rename `@makenowjust-labs/recheck` package to `recheck` ([#141](https://github.com/makenowjust-labs/recheck/pull/139))

Fixes:

- Update `sourcecode` to 0.2.7 ([#72](https://github.com/makenowjust-labs/recheck/pull/72))
- Update Scala version to 2.13.6
- Run `recheck batch` requests in asynchronous ([#89](https://github.com/makenowjust-labs/recheck/pull/89))
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
