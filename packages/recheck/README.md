# `recheck`

> A vulnerable RegExp ([ReDoS][]) checker for JavaScript ([ECMA-262][]) RegExp.

[redos]: https://en.wikipedia.org/wiki/ReDoS
[ecma-262]: https://www.ecma-international.org/ecma-262/11.0/index.html#title

## Installation

Uses the following command to install this library:

```console
$ npm install recheck
```

## Usage

This library exports two APIs. The first is `check`, and another is `checkSync`.
They take a RegExp pattern source and flags to be checked (and parameters optionally),
then it returns the analysis result.

```javascript
const { check } = require("recheck");

console.log(await check("^(a|a)*$", ""));
// {
//   source: '^(a|a)*$',
//   flags: '',
//   status: 'vulnerable',
//   checker: 'automaton',
//   attack: {
//     pumps: [ { prefix: 'a', pump: 'a', bias: 0 } ],
//     suffix: '\x00',
//     base: 27,
//     string: 'aaaaaaaaaaaaaaaaaaaaaaaaaaaa\x00',
//     pattern: "'a' + 'a'.repeat(27) + '\\x00'"
//   },
//   complexity: { type: 'exponential', summary: 'exponential', isFuzz: false },
//   hotspot: [
//     { start: 2, end: 3, temperature: 'heat' },
//     { start: 4, end: 5, temperature: 'heat' }
//   ]
// }
```

Noting that `checkSync` is synchronous version of `check`, it blocks JavaScript process.

### Configuration

Optional parameters specify as the 3rd argument of the `check` function.

```javascript
console.log(await check("^(a|a)*$", "", { timeout: 1000, checker: "fuzz" }));
```

The following parameters are available.

#### `checker`

Type: `'auto' | 'fuzz' | 'automaton'`

Default: `'auto'`

Type of checker used for analysis.

There are three checkers:

- `'automaton'`: A checker which works based on automaton theory.
  It can analyze ReDoS vulnerability of the RegExp without false positive,
  however, it needs some minutes against some RegExp and it does not support some syntax.
- `'fuzz'`: A checker based on fuzzing.
  It can detect ReDoS vulnerability against the all RegExp syntax including back-references
  and look-around assertions. However, it needs some seconds on average and it may cause false
  negative.
- `'auto'`: A checker which combines the automaton checker and the fuzzing checker.
  If the RegExp is supported by the automaton checker and some thresholds are passed,
  it uses the automaton checker. Otherwise, it falls back to the fuzzing checker.

The auto checker performs better than others in many cases.

#### `timeout`

Type: `number | null`

Default: `10000`

Upper limit of analysis time.

If the analysis time exceeds this value, the result will be reported as a timeout.  
If the value is the positive infinite duration, the result never become a timeout.

If the `number` value is specified, it is parsed in milliseconds.
If the value is `null`, it is parsed as the positive infinite duration.

#### `logger`

Type: `((message: string) => void) | null`

Default: `null`

Logger to log an analysis execution.

<details>

<summary>Other parameters to specify detailed behavior</summary>

And, there are other parameters to specify detailed behavior.
They are set to perform better as the default, so it is rare to specify them
and it needs to know the checkers in depth to set the correct value.

#### `maxAttackStringSize`

Type: `number`

Default: `300000`

Maximum length of an attack string.

#### `attackLimit`

Type: `number`

Default: `1500000000`

Upper limit on the number of characters read by the VM during attack string construction.

#### `randomSeed`

Type: `number`

Default: `0`

Seed value for PRNG used by fuzzing.

#### `maxIteration`

Type: `number`

Default: `10`

Maximum number of iterations of genetic algorithm.

#### `seeder`

Type: `'static' | 'dynamic'`

Default: `'static'`

Type of seeder used for constructing the initial generation of fuzzing.

There are two seeders:

- `'static'`: Seeder to construct the initial generation by using static analysis to the given pattern.
- `'dynamic'`: Seeder to construct the initial generation by using dynamic analysis to the given pattern.

#### `maxSimpleRepeatCount`

Type: `number`

Default: `30`

Maximum number of sum of repeat counts for static seeder.

#### `seedingLimit`

Type: `number`

Default: `1000`

Upper limit on the number of characters read by the VM during seeding.

#### `seedingTimeout`

Type: `number | null`

Default: `100`

Upper limit of VM execution time during seeding.

If the `number` value is specified, it is parsed in milliseconds.
If the value is `null`, it is parsed as the positive infinite duration.

#### `maxInitialGenerationSize`

Type: `number`

Default: `500`

Maximum population at the initial generation.

#### `incubationLimit`

Type: `number`

Default: `25000`

Upper limit on the number of characters read by the VM during incubation.

#### `incubationTimeout`

Type: `number | null`

Default: `250`

Upper limit of VM execution time during incubation.

If the `number` value is specified, it is parsed in milliseconds.
If the value is `null`, it is parsed as the positive infinite duration.

#### `maxGeneStringSize`

Type: `number`

Default: `2400`

Maximum length of an attack string on genetic algorithm iterations.

#### `maxGenerationSize`

Type: `number`

Default: `100`

Maximum population at a single generation.

#### `crossoverSize`

Type: `number`

Default: `25`

Number of crossovers in a single generation.

#### `mutationSize`

Type: `number`

Default: `50`

Number of mutations in a single generation.

#### `attackTimeout`

Type: `number | null`

Default: `1000`

The upper limit of the VM execution time when constructing a attack string.

If the execution time exceeds this value, the result will be reported as a vulnerable.

If the `number` value is specified, it is parsed in milliseconds.
If the value is `null`, it is parsed as the positive infinite duration.

#### `maxDegree`

Type: `number`

Default: `4`

Maximum degree for constructing attack string.

#### `heatRatio`

Type: `number`

Default: `0.001`

Ratio of the number of characters read to the maximum number to be considered a hotspot.

#### `accelerationMode`

Type: `'auto' | 'on' | 'off'`

Default: `'auto'`

Mode of acceleration of VM execution.

There are three mode:

- `'auto'`: The automatic mode.
  When it is specified, VM acceleration is used for regular expressions contains no back-reference,
  because back-reference makes VM acceleration slow sometimes.
- `'on'`: The force **on** mode.
- `'off'`: The force **off** mode.

#### `maxRecallStringSize`

Type: `number`

Default: `300000`

Maximum length of an attack string on the recall validation.

#### `recallLimit`

Type: `number`

Default: `1500000000`

Upper limit on the number of characters read on the recall validation.

#### `recallTimeout`

Type: `number | null`

Default: `-1`

Upper limit of recall validation time.

If the recall validation time exceeds this value, the validation is succeeded.
If the negative value is specified, the validation succeeds immediately.

If the `number` value is specified, it is parsed in milliseconds.
If the value is `null`, it is parsed as the positive infinite duration.

Note that Scala.js does not support the recall validation for now.
Please set negative value in this case.

#### `maxRepeatCount`

Type: `number`

Default: `30`

Maximum number of sum of repeat counts.

If this value is exceeded, it switches to use the fuzzing checker.

#### `maxNFASize`

Type: `number`

Default: `35000`

Maximum transition size of NFA to use the automaton checker.

If transition size of NFA (and also DFA because it is larger in general) exceeds this value,
it switches to use the fuzzing checker.

#### `maxPatternSize`

Type: `number`

Default: `1500`

Maximum pattern size to use the automaton checker.

If this value is exceeded, it switches to use the fuzzing checker.

</details>

### Environment Variables

`recheck` package recognizes the following environment variables:

#### `RECHECK_BACKEND`

Type: `'auto' | 'java' | 'native' | 'pure'`

Default: `'auto'`

This variable specifies the backend of the checker implementation.

- `'auto'`: Tries `java` and `native` sequentially. If available implementation is found, it uses this.
  Otherwise, it uses `pure` implementation as fallback.
- `'java'`: Java implementation backend (`recheck-jar` package).
- `'native'`: Native implementation backend (`recheck-${os}-${arch}` package).
- `'pure'`: Pure JavaScript (Scala.js) implementation.

#### `RECHECK_JAR`

Type: `string | null`

Default: `null`

The path of `recheck.jar` archive. If it is `null` or empty string, it uses `recheck-jar` package.

#### `RECHECK_BIN`

Type: `string | null`

Default: `null`

The path of `recheck` (or `recheck.exe`) binary. If it is `null` or empty string, it uses `recheck-${os}-${arch}` package.

## License

MIT License.

2020-2022 (C) TSUYUSATO "MakeNowJust" Kitsune
