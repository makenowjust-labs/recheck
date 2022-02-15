/**
 * Checks whether the given RegExp pattern is ReDoS vulnerable or safe.
 * It takes an optional argument to specify parameters.
 */
export function check(
  source: string,
  flags: string,
  params?: Parameters & HasAbortSignal
): Promise<Diagnostics>;

/**
 * Synchronous version of `check`. Take care that it will block the Node.js process.
 */
export function checkSync(
  source: string,
  flags: string,
  params?: Parameters
): Diagnostics;

/**
 * Parameters is parameters for analysis.
 */
export type Parameters = {
  /**
   * The type of checker to be used.
   *
   * There are three checker types.
   *
   * - `auto` checker uses the criteria to decide which algorithm is better to use against a regular expression, the algorithm based on automata theory or the fuzzing algorithm.
   * - `fuzz` checker uses the fuzzing algorithm with static analysis.
   * - `automaton` checker uses the algorithm based on automata theory.
   *
   * (default: `'auto'`)
   */
  checker?: "auto" | "fuzz" | "automaton";

  /**
   * The upper limit of checking time.
   *
   * If the checking time exceeds this limit, the result will be reported as `timeout`. If the value is positive infinite in Scala or `null` in TypeScript, the result never becomes `timeout`.
   *
   * The `timeout` time begins to be measured as soon as the check starts. Note that the `timeout` does not occur while the input is in the queue waiting to be checked.
   *
   * In TypeScript, a number value is treated as in milliseconds.
   *
   * (default: `10000`)
   */
  timeout?: number | null;

  /**
   * The logger function to record execution traces.
   *
   * To disable the logging, `null` in TypeScript or `None` in Scala should be passed.
   *
   * (default: `null`)
   */
  logger?: (message: string) => void;

  /**
   * The PRNG seed number.
   *
   * (default: `0`)
   */
  randomSeed?: number;

  /**
   * The maximum number of fuzzing iteration.
   *
   * (default: `10`)
   */
  maxIteration?: number;

  /**
   * The type of seeder to be used in fuzzing.
   *
   * There are two seeders.
   *
   * - `static` seeder uses the seeding algorithm based on the automata theory.
   * - `dynamic` seeder uses the seeding algorithm with dynamic analysis.
   *
   * (default: `'static'`)
   */
  seeder?: "static" | "dynamic";

  /**
   * The maximum number of each repetition quantifier’s repeat count on `static` seeding.
   *
   * (default: `30`)
   */
  maxSimpleRepeatCount?: number;

  /**
   * The upper limit on the number of characters read by VM on `dynamic` seeding.
   *
   * (default: `1000`)
   */
  seedingLimit?: number;

  /**
   * The upper limit of matching time on `dynamic` seeding.
   *
   * (default: `100`)
   */
  seedingTimeout?: number | null;

  /**
   * The maximum size of the initial generation on fuzzing.
   *
   * (default: `500`)
   */
  maxInitialGenerationSize?: number;

  /**
   * The upper limit on the number of characters read by VM on incubation.
   *
   * (default: `25000`)
   */
  incubationLimit?: number;

  /**
   * The upper limit of matching time on incubation.
   *
   * (default: `250`)
   */
  incubationTimeout?: number | null;

  /**
   * The maximum length of the gene string on fuzzing.
   *
   * (default: `2400`)
   */
  maxGeneStringSize?: number;

  /**
   * The maximum size of each generation on fuzzing.
   *
   * (default: `100`)
   */
  maxGenerationSize?: number;

  /**
   * The number of crossover on each generation.
   *
   * (default: `25`)
   */
  crossoverSize?: number;

  /**
   * The number of mutation on each generation.
   *
   * (default: `50`)
   */
  mutationSize?: number;

  /**
   * The upper limit on the number of characters read by VM on the attack.
   *
   * (default: `1500000000`)
   */
  attackLimit?: number;

  /**
   * The upper limit of matching time on the attack.
   *
   * (default: `1000`)
   */
  attackTimeout?: number | null;

  /**
   * The maximum length of the attack string on fuzzing.
   *
   * (default: `300000`)
   */
  maxAttackStringSize?: number;

  /**
   * The maximum degree to be considered in fuzzing.
   *
   * (default: `4`)
   */
  maxDegree?: number;

  /**
   * The ratio of the number of characters read to the maximum number to be considered as a hot spot.
   *
   * (default: `0.001`)
   */
  heatRatio?: number;

  /**
   * The type of acceleration mode strategy on fuzzing.
   *
   * There are three acceleration mode strategies.
   *
   * - `auto` uses acceleration mode as default. However, if the regular expression has backreferences, it turns off the acceleration mode.
   * - `on` turns on the acceleration mode.
   * - `off` turns off the acceleration mode.
   *
   * (default: `'auto'`)
   */
  accelerationMode?: "auto" | "on" | "off";

  /**
   * The maximum number of sum of repetition quantifier’s repeat counts to determine which algorithm is used.
   *
   * (default: `30`)
   */
  maxRepeatCount?: number;

  /**
   * The maximum size of the regular expression pattern to determine which algorithm is used.
   *
   * (default: `1500`)
   */
  maxPatternSize?: number;

  /**
   * The maximum size of NFA to determine which algorithm is used.
   *
   * (default: `35000`)
   */
  maxNFASize?: number;

  /**
   * The upper limit on the number of characters read by VM on the recall validation.
   *
   * (default: `1500000000`)
   */
  recallLimit?: number;

  /**
   * The upper limit of matching time on the recall validation.
   *
   * If this value is negative, then the recall validation is skipped.
   *
   * (default: `-1000`)
   */
  recallTimeout?: number | null;

  /**
   * The maximum length of the attack string on recall validation.
   *
   * (default: `300000`)
   */
  maxRecallStringSize?: number;
};

/**
 * HasAbortSignal is a mix-in type for having `signal` field.
 */
export type HasAbortSignal = {
  /**
   * Signal to abort the check.
   */
  signal?: AbortSignal;
};

/**
 * Diagnostics is an analyzing result.
 * It takes one of the following three statuses:
 *
 * - `safe`: The given RegExp is safe.
 * - `vulnerable`: The given RegExp is potentially vulnerable.
 * - `unknown`: An error is occurred on analyzing.
 *   As a result, it is unknown whether the RegExp is safe or vulnerable.
 */
export type Diagnostics =
  | SafeDiagnostics
  | VulnerableDiagnostics
  | UnknownDiagnostics;

/**
 * SafeDiagnostics is a diagnostics against a safe RegExp.
 */
export type SafeDiagnostics = {
  source: string;
  flags: string;
  status: "safe";
  checker: "automaton" | "fuzz";
  complexity: SafeComplexity;
};

/**
 * VulnerableDiagnostics is a diagnostics against a vulnerable RegExp.
 */
export type VulnerableDiagnostics = {
  source: string;
  flags: string;
  status: "vulnerable";
  checker: "automaton" | "fuzz";
  attack: AttackPattern;
  complexity: VulnerableComplexity;
  hotspot: Hotspot[];
};

/**
 * UnknownDiagnostics is a diagnostics when an error is occurred on analyzing.
 */
export type UnknownDiagnostics = {
  source: string;
  flags: string;
  status: "unknown";
  checker?: "automaton" | "fuzz";
  error: Error;
};

/**
 * SafeComplexity is a safe complexity.
 */
export type SafeComplexity = {
  type: "constant" | "linear" | "safe";
  summary: string;
  isFuzz: boolean;
};

/**
 * VulnerableComplexity is a vulnerable complexity.
 */
export type VulnerableComplexity = PolynomialComplexity | ExponentialComplexity;

/**
 * PolynomialComplexity is a polynomial (super-linear) complexity.
 * This complexity is vulnerable typically.
 */
export type PolynomialComplexity = {
  type: "polynomial";
  summary: string;
  degree: number;
  isFuzz: boolean;
};

/**
 * ExponentialComplexity is an exponential complexity.
 * This complexity is vulnerable very.
 */
export type ExponentialComplexity = {
  type: "exponential";
  summary: string;
  isFuzz: boolean;
};

/**
 * Hotspot is a hotspot of the RegExp pattern.
 */
export type Hotspot = {
  start: number;
  end: number;
  temperature: "heat" | "normal";
};

/**
 * Error is a possible error on analyzing.
 */
export type Error = Timeout | Cancel | Unsupported | Invalid | Unexpected;

/**
 * Timeout is a timeout error.
 */
export type Timeout = { kind: "timeout" };

/** Cancel is a cancel error. */
export type Cancel = { kind: "cancel" };

/**
 * Unsupported is an error reported when the RegExp is not supported.
 */
export type Unsupported = {
  kind: "unsupported";
  message: string;
};

/**
 * Invalid is an error reported when the RegExp is invalid.
 */
export type Invalid = {
  kind: "invalid";
  message: string;
};
/**
 * Unexpected is an error reported when unexpected error occurs.
 */
export type Unexpected = {
  kind: "unexpected";
  message: string;
};

/**
 * AttackPattern is an attack pattern string.
 */
export type AttackPattern = {
  pumps: {
    prefix: string;
    pump: string;
    bias: number;
  }[];
  suffix: string;
  base: number;
  string: string;
  pattern: string;
};
