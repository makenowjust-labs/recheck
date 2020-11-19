/**
 * Checks whether the given RegExp pattern is ReDoS vulnerable or safe.
 * It takes an optional argument to specify a configuration.
 */
export function check(
  source: string,
  flags: string,
  config?: Config
): Diagnostics;

/**
 * Config is a configuration parameter of analyzing.
 */
export type Config = {
  /**
   * An integer value of timeout duration milliseconds. (default: `undefined`)
   *
   * When `undefined` is specified, it means there is no timeout.
   */
  timeout?: number | undefined;

  /**
   * A checker name to use. (default: `'hybrid'`)
   *
   * There are three checkers:
   *
   * - `'automaton'`: A checker which works based on automaton theory.
   *   It can analyze ReDoS vulnerability of the RegExp without false positive,
   *   however, it needs some minutes against some RegExps and it does not support some syntax.
   * - `'fuzz'`: A checker based on fuzzing.
   *   It can detect ReDoS vulnerability against the all RegExp syntax including back-references
   *   and look-around assertions. However, it needs some seconds on average and it may cause false
   *   negative.
   * - `'hybrid'`: A checker which combines the automaton based checker and the fuzz checker.
   *   If the RegExp is supported by the automaton based checker and some thresholds are passed,
   *   it uses the automaton based checker. Otherwise, it falls back to the fuzz checker.
   *
   * The hybrid checker performs better than others in many cases.
   */
  checker: "hybrid" | "automaton" | "fuzz";

  /**
   * An integer value of a maximum length of an attack string. (default: `10_000`)
   *
   * The checker finds a vulnerable string not to exceed this length.
   */
  maxAttackSize?: number;

  /**
   * An integer value of a limit of VM execution steps. (default: `1_000_000`)
   *
   * The checker assumes the RegExp is vulnerable when a string exists
   * against which steps exceed the limit.
   */
  attackLimit?: number;

  /**
   * A floating-point number value which represents a ratio of a character
   * to VM execution steps. (default: `1.5`)
   *
   * It is used to build an attack string from the complexity witness.
   */
  stepRate?: number;

  /**
   * An integer value of seed for pseudo-random number generator in fuzzing.
   * (default: `undefined`)
   *
   * When `undefined` is specified, it uses a system default seed.
   */
  randomSeed?: number | undefined;

  /**
   * An integer value of a limit of VM execution steps on the seeding phase.
   * (default: `10_000`)
   */
  seedLimit?: number;

  /**
   * An integer value of a limit of VM execution steps on the incubation phase.
   * (default: `100_000`)
   */
  populationLimit?: number;

  /**
   * An integer value of the number of crossings on one generation. (default: `25`)
   */
  crossSize?: number;

  /**
   * An integer value of the number of mutations on one generation. (default: `50`)
   */
  mutateSize?: number;

  /**
   * An integer value of a maximum size of a seed set. (default: `100`)
   */
  maxSeedSize?: number;

  /**
   * An integer value of a maximum size of a living population on one generation.
   * (default: `100`)
   */
  maxGenerationSize?: number;

  /**
   * An integer value of a number of iterations on the incubation phase. (default: `30`)
   */
  maxIteration?: number;

  /**
   * An integer value of a limit of repetition count in the RegExp. (default: `10`)
   *
   * If the RegExp exceeds this limit on the hybrid checker, it switches to use
   * the fuzz checker to analyze instead of the automaton based checker.
   */
  maxRepeatCount?: number;

  /**
   * An integer value of a maximum size of the transition function of NFA. (default: `1000`)
   *
   * If the NFA's transition function exceeds this limit on the hybrid checker,
   * it switches to use fuzz checker to analyze instead of the automaton based checker.
   */
  maxNFASize?: number;
};

/**
 * Diagnostics is an analyzing result.
 * It takes one of the following three statuses:
 *
 * - `safe`: The given RegExp is safe.
 * - `vulnerable`: The given RegExp is potentially vulnerable.
 * - `unknown`: An error is occured on analyzing.
 *   As a result, it is unknown wheher the RegExp is safe or vulnerable.
 */
export type Diagnostics = Safe | Vulnerable | Unknown;

/**
 * Safe is a diagnostics against a safe RegExp.
 */
export type Safe = {
  status: "safe";
  complexity?: Constant | Linear;
};

/**
 * Vulnerable is a diagnostics against a vulnerable RegExp.
 */
export type Vulnerable = {
  status: "vulnerable";
  attack: string;
  complexity?: Polynomial | Exponential;
};

/**
 * Unknown is a diagnostics when an error is occurred on analyzing.
 */
export type Unknown = {
  status: "unknown";
  error: Error;
};

/**
 * Constant is a constant complexity.
 * This complexity is safe.
 */
export type Constant = { type: "constant" };

/**
 * Linear is a linear complexity.
 * This complexity is safe.
 */
export type Linear = { type: "linear" };

/**
 * Polynomial is a polynomial (super-linear) complexity.
 * This complexity is vulneable typically.
 */
export type Polynomial = {
  type: "polynomial";
  degree: number;
  witness: Witness;
};

/**
 * Exponential is an exponential complexity.
 * This complexity is vulnerable very.
 */
export type Exponential = {
  type: "exponential";
  witness: Witness;
};

/**
 * Error is a possible error on analyzing.
 */
export type Error = Timeout | Unsupported | Invalid;

/**
 * Timeout is a timeout error.
 */
export type Timeout = { kind: "timeout" };

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
 * Witness is a witness of ReDoS vulnerable complexity.
 * In short, it is a pattern of an attack string pattern.
 */
export type Witness = {
  pumps: {
    prefix: string;
    pump: string;
  }[];
  suffix: string;
};
