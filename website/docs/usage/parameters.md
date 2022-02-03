---
id: parameters
title: Parameters
---

`Parameters` is an object type to specify detailed checker behavior.
It has many parameters.
But, few parameters are useful to users, and the others are not needed to be set manually.

You can look at the whole type definition [here](https://github.com/makenowjust-labs/recheck/blob/main/packages/recheck/index.d.ts) (TypeScript) or [here](https://github.com/makenowjust-labs/recheck/blob/main/modules/recheck-common/shared/src/main/scala/codes/quine/labs/recheck/common/Parameters.scala) (Scala).

## `checker`

- Type: `'auto' | 'fuzz' | 'automaton'` (TypeScript), `Checker` (Scala)
- Default: `'auto'` (TypeScript), `Checker.Auto` (Scala)

The type of checker to be used.

There are three checker types.

- `auto` checker uses the criteria to decide which algorithm is better to use against a regular expression, the algorithm based on automata theory or the fuzzing algorithm.
- `fuzz` checker uses the fuzzing algorithm with static analysis.
- `automaton` checker uses the algorithm based on automata theory.

## `timeout`

- Type: `number | null` (TypeScript), `Duration` (Scala)
- Default: `10000` (TypeScript), `Duration(10, SECONDS)` (Scala)

The upper limit of checking time.

If the checking time exceeds this limit, the result will be reported as `timeout`. If the value is positive infinite in Scala or `null` in TypeScript, the result never becomes `timeout`.

The `timeout` time begins to be measured as soon as the check starts. Note that the `timeout` does not occur while the input is in the queue waiting to be checked.

In TypeScript, a number value is treated as in milliseconds.

## `logger`

- Type: `(message: string) => void` (TypeScript), `Option[Context.Logger]` (Scala)
- Default: `null` (TypeScript), `None` (Scala)

The logger function to record execution traces.

To disable the logging, `null` in TypeScript or `None` in Scala should be passed.

## `randomSeed`

- Type: `number` (TypeScript), `Long` (Scala)
- Default: `0`

The PRNG seed number.

## `maxIteration`

- Type: `number` (TypeScript), `Int` (Scala)
- Default: `10`

The maximum number of fuzzing iteration.

## `seeder`

- Type: `'static' | 'dynamic'` (TypeScript), `Seeder` (Scala)
- Default: `'static'` (TypeScript), `Seeder.Static` (Scala)

The type of seeder to be used in fuzzing.

There are two seeders.

- `static` seeder uses the seeding algorithm based on the automata theory.
- `dynamic` seeder uses the seeding algorithm with dynamic analysis.

## `maxSimpleRepeatCount`

- Type: `number` (TypeScript), `Int` (Scala)
- Default: `30`

The maximum number of each repetition quantifier’s repeat count on `static` seeding.

## `seedingLimit`

- Type: `number` (TypeScript), `Int` (Scala)
- Default: `1000`

The upper limit on the number of characters read by VM on `dynamic` seeding.

## `seedingTimeout`

- Type: `number | null` (TypeScript), `Duration` (Scala)
- Default: `100` (TypeScript), `Duration(100, MILLISECONDS)` (Scala)

The upper limit of matching time on `dynamic` seeding.

## `maxInitialGenerationSize`

- Type: `number` (TypeScript), `Int` (Scala)
- Default: `500`

The maximum size of the initial generation on fuzzing.

## `incubationLimit`

- Type: `number` (TypeScript), `Int` (Scala)
- Default: `25000`

The upper limit on the number of characters read by VM on incubation.

## `incubationTimeout`

- Type: `number | null` (TypeScript), `Duration` (Scala)
- Default: `250` (TypeScript), `Duration(250, MILLISECONDS)` (Scala)

The upper limit of matching time on incubation.

## `maxGeneStringSize`

- Type: `number` (TypeScript), `Int` (Scala)
- Default: `2400`

The maximum length of the gene string on fuzzing.

## `maxGenerationSize`

- Type: `number` (TypeScript), `Int` (Scala)
- Default: `100`

The maximum size of each generation on fuzzing.

## `crossoverSize`

- Type: `number` (TypeScript), `Int` (Scala)
- Default: `25`

The number of crossover on each generation.

## `mutationSize`

- Type: `number` (TypeScript), `Int` (Scala)
- Default: `50`

The number of mutation on each generation.

## `attackLimit`

- Type: `number` (TypeScript), `Int` (Scala)
- Default: `1500000000`

The upper limit on the number of characters read by VM on the attack.

## `attackTimeout`

- Type: `number | null` (TypeScript), `Duration` (Scala)
- Default: `1000` (TypeScript), `Duration(1, SECONDS)` (Scala)

The upper limit of matching time on the attack.

## `maxAttackStringSize`

- Type: `number` (TypeScript), `Int` (Scala)
- Default: `300000`

The maximum length of the attack string on fuzzing.

## `maxDegree`

- Type: `number` (TypeScript), `Int` (Scala)
- Default: `4`

The maximum degree to be considered in fuzzing.

## `heatRatio`

- Type: `number` (TypeScript), `Double` (Scala)
- Default: `0.001`

The ratio of the number of characters read to the maximum number to be considered as a hot spot.

## `accelerationMode`

- Type: `'auto' | 'on' | 'off'` (TypeScript), `AccelerationMode` (Scala)
- Default: `'auto'` (TypeScript), `AccelerationMode.Auto` (Scala)

The type of acceleration mode strategy on fuzzing.

There are three acceleration mode strategies.

- `auto` uses acceleration mode as default. However, if the regular expression has backreferences, it turns off the acceleration mode.
- `on` turns on the acceleration mode.
- `off` turns off the acceleration mode.

## `maxRepeatCount`

- Type: `number` (TypeScript), `Int` (Scala)
- Default: `30`

The maximum number of sum of repetition quantifier’s repeat counts to determine which algorithm is used.

## `maxPatternSize`

- Type: `number` (TypeScript), `Int` (Scala)
- Default: `1500`

The maximum size of the regular expression pattern to determine which algorithm is used.

## `maxNFASize`

- Type: `number` (TypeScript), `Int` (Scala)
- Default: `35000`

The maximum size of NFA to determine which algorithm is used.

## `recallLimit`

- Type: `number` (TypeScript), `Int` (Scala)
- Default: `1500000000`

The upper limit on the number of characters read by VM on the recall validation.

## `recallTimeout`

- Type: `number | null` (TypeScript), `Duration` (Scala)
- Default: `-1000` (TypeScript), `Duration(-1, SECONDS)` (Scala)

The upper limit of matching time on the recall validation.

If this value is negative, then the recall validation is skipped.

## `maxRecallStringSize`

- Type: `number` (TypeScript), `Int` (Scala)
- Default: `300000`

The maximum length of the attack string on recall validation.
