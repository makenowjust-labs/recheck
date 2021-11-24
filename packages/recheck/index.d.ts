/**
 * Checks whether the given RegExp pattern is ReDoS vulnerable or safe.
 * It takes an optional argument to specify a configuration.
 */
export function check(
  source: string,
  flags: string,
  config?: Config
): Promise<Diagnostics>;

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
   *   however, it needs some minutes against some RegExp and it does not support some syntax.
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
  checker?: "hybrid" | "automaton" | "fuzz";

  /**
   * An integer value of a maximum length of an attack string. (default: `4_000`)
   *
   * The checker finds a vulnerable string not to exceed this length.
   */
  maxAttackSize?: number;

  /**
   * An integer value of a limit of VM execution steps. (default: `100_000`)
   *
   * The checker assumes the RegExp is vulnerable when a string exists
   * against which steps exceed the limit.
   */
  attackLimit?: number;

  /**
   * An integer value of seed for pseudo-random number generator in fuzzing.
   * (default: `undefined`)
   *
   * When `undefined` is specified, it uses a system default seed.
   */
  randomSeed?: number | undefined;

  /**
   * An integer value of a limit of VM execution steps on the seeding phase.
   * (default: `1_000`)
   */
  seedLimit?: number;

  /**
   * An integer value of a limit of VM execution steps on the incubation phase.
   * (default: `10_000`)
   */
  incubationLimit?: number;

  /**
   * An integer value of the number of crossings on one generation. (default: `25`)
   */
  crossSize?: number;

  /**
   * An integer value of the number of mutations on one generation. (default: `50`)
   */
  mutateSize?: number;

  /**
   * An integer value of a maximum size of a seed set. (default: `50`)
   */
  maxSeedSize?: number;

  /**
   * An integer value of a maximum size of a living population on one generation.
   * (default: `100`)
   */
  maxGenerationSize?: number;

  /**
   * An integer value of the number of iterations on the incubation phase. (default: `30`)
   */
  maxIteration?: number;

  /**
   * An integer value of a maximum degree to attempt on building an attack string.
   * (default: `4`)
   */
  maxDegree?: number;

  /**
   * A rate of a hotspot steps by the maximum steps.
   * (default: `0.001`)
   */
  heatRate?: number;

  /**
   * Whether to use acceleration or not on fuzzing.
   * (default: `true`)
   */
  usesAcceleration?: boolean;

  /**
   * An integer value of a limit of repetition count in the RegExp. (default: `20`)
   *
   * If the RegExp exceeds this limit on the hybrid checker, it switches to use
   * the fuzz checker to analyze instead of the automaton based checker.
   */
  maxRepeatCount?: number;

  /**
   * An integer value of a maximum size of the transition function of NFA. (default: `40000`)
   *
   * If the NFA's transition function exceeds this limit on the hybrid checker,
   * it switches to use fuzz checker to analyze instead of the automaton based checker.
   */
  maxNFASize?: number;

  /**
   * An integer value of maximum size of the pattern. (default: `1500`)
   *
   * If the pattern size exceeds this limit on the hybrid checker,
   * it switches to use fuzz checker to analyze instead of the automaton based checker.
   */
  maxPatternSize?: number;

  /**
   * An signal to abort the check.
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
  degree: number;
  isFuzz: boolean;
};

/**
 * ExponentialComplexity is an exponential complexity.
 * This complexity is vulnerable very.
 */
export type ExponentialComplexity = {
  type: "exponential";
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
export type Error = Timeout | Cancel | Unsupported | Invalid;

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
};
