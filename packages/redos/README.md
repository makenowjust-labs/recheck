# `@makenowjust-labo/redos`

> A vulnerable RegExp ([ReDoS][]) checker for JavaScript ([ECMA-262][]) RegExp.

[redos]: https://en.wikipedia.org/wiki/ReDoS
[ecma-262]: https://www.ecma-international.org/ecma-262/11.0/index.html#title

## Installation

Uses the following command to install this library:

```console
$ npm install @makenowjust-labo/redos
```

## Usage

This library exports only the API called `check`.
It takes a RegExp pattern source and flags to be checked (and a configuration optionally),
then it returns the analysis result.

```javascript
const { check } = require("@makenowjust-labo/redos");

console.log(check("^(a|a)*$", ""));
// {
//   status: 'vulnerable',
//   attack: 'aaaaaaaaaaaaaaaaaaaaa\x00',
//   complexity: {
//     type: 'exponential',
//     witness: { pumps: [ { prefix: 'a', pump: 'a' } ], suffix: '\x00' }
//   }
// }
```

### Configuration

A configuration parameter specifies as the 3rd argument of the `check` function.

```javascript
console.log(check("^(a|a)*$", "", { timeout: 1000, checker: "fuzz" }));
```

The following parameters are available.

#### `timeout`

An integer value of timeout duration milliseconds. (default: `undefined`)

When `undefined` is specified, it means there is no timeout.

#### `checker`

A checker name to use. (default: `'hybrid'`)

There are three checkers:

- `'automaton'`: A checker which works based on automaton theory.
  It can analyze a ReDoS vulnerability of the RegExp without false positives,
  however, it needs some minutes against some RegExps and it does not support some syntax.
- `'fuzz'`: A checker based on fuzzing.
  It can detect a ReDoS vulnerability against all RegExp syntax including back-references
  and look-around assertions. However, it needs some seconds on average and it may cause
  false negatives.
- `'hybrid'`: A checker which combines the automaton based checker and the fuzz checker.
  If the RegExp is supported by the automaton based checker and some thresholds are passed,
  it uses the automaton based checker. Otherwise, it falls back to the fuzz checker.

The hybrid checker performs better than others in many cases.

<details>

<summary>Other parameters to specify detailed behavior</summary>

And, there are other parameters to specify detailed behavior.
They are set to perform better as the default, so it is rare to specify them
and it needs to know the checkers in depth to set the correct value.

#### `maxAttackSize`

An integer value of a maximum length of an attack string. (default: `10_000`)

The checker finds a vulnerable string not to exceed this length.

#### `attackLimit`

An integer value of a limit of VM execution steps. (default: `1_000_000`)

The checker assumes the RegExp is vulnerable when a string exists
against which steps exceed the limit.

#### `stepRate`

A floating-point number value which represents a ratio of a character to VM execution steps. (default: `1.5`)

It is used to build an attack string from the complexity witness.

#### `randomSeed`

An integer value of seed for pseudo-random number generator in fuzzing. (default: `undefined`)

When `undefined` is specified, it uses a system default seed.

#### `seedLimit`

An integer value of a limit of VM execution steps on the seeding phase. (default: `10_000`)

#### `populationLimit`

An integer value of a limit of VM execution steps on the incubation phase. (default: `100_000`)

#### `crossSize`

An integer value of the number of crossings on one generation. (default: `25`)

#### `mutateSize`

An integer value of the number of mutations on one generation. (default: `50`)

#### `maxSeedSize`

An integer value of a maximum size of a seed set. (default: `100`)

#### `maxGenerationSize`

An integer value of a maximum size of a living population on one generation. (default: `100`)

#### `maxIteration`

An integer value of a number of iterations on the incubation phase. (default: `30`)

#### `maxRepeatCount`

An integer value of a limit of repetition count in the RegExp. (default: `10`)

If the RegExp exceeds this limit on the hybrid checker, it switches to
use the fuzz checker to analyze instead of the automaton based checker.

#### `maxNFASize`

An integer value of a maximum size of the transition function of NFA. (default: `1000`)

If the NFA's transition function exceeds this limit on the hybrid checker,
it switches to use fuzz checker to analyze instead of the automaton based checker.

</details>

## License

MIT License.

2020 (C) TSUYUSATO "MakeNowJust" Kitsune
