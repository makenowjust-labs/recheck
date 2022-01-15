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
   * Type of checker used for analysis.
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
   * - `'auto'`: A checker which combines the automaton checker and the fuzzing checker.
   *   If the RegExp is supported by the automaton checker and some thresholds are passed,
   *   it uses the automaton checker. Otherwise, it falls back to the fuzzing checker.
   *
   * The auto checker performs better than others in many cases.
   *
   * (default: `'auto'`)
   */
  checker?: "auto" | "fuzz" | "automaton";

  /**
   * Upper limit of analysis time.
   *
   * If the analysis time exceeds this value, the result will be reported as a timeout.
   * If the value is the positive infinite duration, the result never become a timeout.
   *
   * If the `number` value is specified, it is parsed in milliseconds.
   * If the value is `null`, it is parsed as the positive infinite duration.
   *
   *
   * (default: `10000`)
   */
  timeout?: number | null;

  /**
   * Logger to log an analysis execution.
   *
   * (default: `null`)
   */
  logger?: ((message: string) => void) | null;

  /**
   * Maximum length of an attack string.
   *
   * (default: `300000`)
   */
  maxAttackStringSize?: number;

  /**
   * Upper limit on the number of characters read by the VM during attack string construction.
   *
   * (default: `1500000000`)
   */
  attackLimit?: number;

  /**
   * Seed value for PRNG used by fuzzing.
   *
   * (default: `0`)
   */
  randomSeed?: number;

  /**
   * Maximum number of iterations of genetic algorithm.
   *
   * (default: `10`)
   */
  maxIteration?: number;

  /**
   * Type of seeder used for constructing the initial generation of fuzzing.
   *
   * There are two seeders:
   *
   * - `'static'`: Seeder to construct the initial generation by using static analysis to the given pattern.
   * - `'dynamic'`: Seeder to construct the initial generation by using dynamic analysis to the given pattern.
   *
   * (default: `'static'`)
   */
  seeder?: "static" | "dynamic";

  /**
   * Maximum number of sum of repeat counts for static seeder.
   *
   * (default: `30`)
   */
  maxSimpleRepeatCount?: number;

  /**
   * Upper limit on the number of characters read by the VM during seeding.
   *
   * (default: `1000`)
   */
  seedingLimit?: number;

  /**
   * Upper limit of VM execution time during seeding.
   *
   * If the `number` value is specified, it is parsed in milliseconds.
   * If the value is `null`, it is parsed as the positive infinite duration.
   *
   *
   * (default: `100`)
   */
  seedingTimeout?: number | null;

  /**
   * Maximum population at the initial generation.
   *
   * (default: `500`)
   */
  maxInitialGenerationSize?: number;

  /**
   * Upper limit on the number of characters read by the VM during incubation.
   *
   * (default: `25000`)
   */
  incubationLimit?: number;

  /**
   * Upper limit of VM execution time during incubation.
   *
   * If the `number` value is specified, it is parsed in milliseconds.
   * If the value is `null`, it is parsed as the positive infinite duration.
   *
   *
   * (default: `250`)
   */
  incubationTimeout?: number | null;

  /**
   * Maximum length of an attack string on genetic algorithm iterations.
   *
   * (default: `2400`)
   */
  maxGeneStringSize?: number;

  /**
   * Maximum population at a single generation.
   *
   * (default: `100`)
   */
  maxGenerationSize?: number;

  /**
   * Number of crossovers in a single generation.
   *
   * (default: `25`)
   */
  crossoverSize?: number;

  /**
   * Number of mutations in a single generation.
   *
   * (default: `50`)
   */
  mutationSize?: number;

  /**
   * The upper limit of the VM execution time when constructing a attack string.
   *
   * If the execution time exceeds this value, the result will be reported as a vulnerable.
   *
   * If the `number` value is specified, it is parsed in milliseconds.
   * If the value is `null`, it is parsed as the positive infinite duration.
   *
   *
   * (default: `1000`)
   */
  attackTimeout?: number | null;

  /**
   * Maximum degree for constructing attack string.
   *
   * (default: `4`)
   */
  maxDegree?: number;

  /**
   * Ratio of the number of characters read to the maximum number to be considered a hotspot.
   *
   * (default: `0.001`)
   */
  heatRatio?: number;

  /**
   * Mode of acceleration of VM execution.
   *
   * There are three mode:
   *
   * - `'auto'`: The automatic mode.
   *   When it is specified, VM acceleration is used for regular expressions contains no back-reference,
   *   because back-reference makes VM acceleration slow sometimes.
   * - `'on'`: The force **on** mode.
   * - `'off'`: The force **off** mode.
   *
   * (default: `'auto'`)
   */
  accelerationMode?: "auto" | "on" | "off";

  /**
   * Maximum length of an attack string on the recall validation.
   *
   * (default: `300000`)
   */
  maxRecallStringSize?: number;

  /**
   * Upper limit on the number of characters read on the recall validation.
   *
   * (default: `1500000000`)
   */
  recallLimit?: number;

  /**
   * Upper limit of recall validation time.
   *
   * If the recall validation time exceeds this value, the validation is succeeded.
   * If the negative value is specified, the validation succeeds immediately.
   *
   * If the `number` value is specified, it is parsed in milliseconds.
   * If the value is `null`, it is parsed as the positive infinite duration.
   *
   * Note that Scala.js does not support the recall validation for now.
   * Please set negative value in this case.
   *
   *
   * (default: `-1`)
   */
  recallTimeout?: number | null;

  /**
   * Maximum number of sum of repeat counts.
   *
   * If this value is exceeded, it switches to use the fuzzing checker.
   *
   * (default: `30`)
   */
  maxRepeatCount?: number;

  /**
   * Maximum transition size of NFA to use the automaton checker.
   *
   * If transition size of NFA (and also DFA because it is larger in general) exceeds this value,
   * it switches to use the fuzzing checker.
   *
   * (default: `35000`)
   */
  maxNFASize?: number;

  /**
   * Maximum pattern size to use the automaton checker.
   *
   * If this value is exceeded, it switches to use the fuzzing checker.
   *
   * (default: `1500`)
   */
  maxPatternSize?: number;
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
