/**
 * Checks whether the given RegExp pattern is ReDoS vulnerable or safe.
 * It takes an optional argument to specify parameters.
 */
export function check(
  source: string,
  flags: string,
  params?: Parameters & HasAbortSignal,
): Promise<Diagnostics>;

/**
 * Synchronous version of `check`. Take care that it will block the Node.js process.
 */
export function checkSync(
  source: string,
  flags: string,
  params?: Parameters,
): Diagnostics;

/**
 * Checks whether the given RegExp pattern is ReDoS vulnerable or safe, and returns all the diagnostics found until the reported one.
 *
 * The list consists of the vulnerable diagnostics whose recall validation failed, followed by the diagnostics that `check` returns.
 * It is longer than one only when the recall validation runs (see the `recallTimeout` parameter).
 */
export function checkAll(
  source: string,
  flags: string,
  params?: Parameters & HasAbortSignal,
): Promise<Diagnostics[]>;

/**
 * Synchronous version of `checkAll`. Take care that it will block the Node.js process.
 */
export function checkAllSync(
  source: string,
  flags: string,
  params?: Parameters,
): Diagnostics[];

export type {
  Dialect,
  DialectParameter,
  DialectSettings,
  Parameters,
} from "./parameters";

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
  SafeDiagnostics | VulnerableDiagnostics | UnknownDiagnostics;

/**
 * SafeDiagnostics is a diagnostics against a safe RegExp.
 */
export type SafeDiagnostics = {
  source: string;
  flags: string;
  dialect: Dialect;
  status: "safe";
  complexity: SafeComplexity;
  checker: "automaton" | "fuzz";
};

/**
 * VulnerableDiagnostics is a diagnostics against a vulnerable RegExp.
 */
export type VulnerableDiagnostics = {
  source: string;
  flags: string;
  dialect: Dialect;
  /**
   * The unit of the attack string: UTF-16 code units (`utf16`), code points (`code-point`), or bytes (`byte`).
   */
  unitMode: "utf16" | "code-point" | "byte";
  status: "vulnerable";
  complexity: VulnerableComplexity;
  attack: AttackPattern;
  hotspot: Hotspot[];
  /**
   * The recall validation of the attack string, or `null` if it is not run.
   */
  recallValidation: RecallValidation | null;
  checker: "automaton" | "fuzz";
};

/**
 * RecallValidation is a run of the attack string on the real regex engine of the dialect.
 */
export type RecallValidation = {
  /**
   * The runtime name and version, e.g. `"node v26.11.1"`.
   */
  runtime: string;
  /**
   * The timeout of the matching in milliseconds, or `null` if it is infinite.
   */
  timeout: number | null;
  result: RecallResult;
  /**
   * Whether the attack is effective on the engine (`timeout` or `engine-limit`).
   * `check` reports the vulnerable diagnostics only if it is `true`.
   */
  isSucceeded: boolean;
};

/**
 * RecallResult is the result of matching the attack string on the real regex engine.
 *
 * - `finish`: The matching finished in `time` milliseconds, so the attack is not effective.
 * - `timeout`: The matching did not finish in time.
 * - `engine-limit`: The engine stopped on its limit of the matching steps, e.g. PHP's backtrack limit, so the attack is effective.
 * - `engine-error`: The engine gave up with an error on the stack depth or the memory, e.g. Java's `StackOverflowError`.
 *   It may happen on a long string even if the matching takes linear time, so the attack is not considered effective.
 */
export type RecallResult =
  | { kind: "finish"; time: number }
  | { kind: "timeout" }
  | { kind: "engine-limit"; message: string }
  | { kind: "engine-error"; message: string };

/**
 * UnknownDiagnostics is a diagnostics when an error is occurred on analyzing.
 */
export type UnknownDiagnostics = {
  source: string;
  flags: string;
  dialect: Dialect;
  status: "unknown";
  error: Error;
  checker?: "automaton" | "fuzz";
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
export type Error =
  Timeout | Cancel | Unsupported | Invalid | RecallFailureLimit | Unexpected;

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
 * RecallFailureLimit is an error reported when the recall validation fails `maxRecallFailures` times.
 */
export type RecallFailureLimit = { kind: "recall-failure-limit" };

/**
 * Unexpected is an error reported when unexpected error occurs.
 */
export type Unexpected = {
  kind: "unexpected";
  message: string;
};

/**
 * AttackPattern is an attack pattern string.
 *
 * The strings are UTF-16 code units, or bytes as the characters of the same codes if `unitMode` is `byte`.
 */
export type AttackPattern = {
  pumps: {
    prefix: string;
    pump: string;
    bias: number;
  }[];
  suffix: string;
  base: number;
  /**
   * The attack string, i.e. `prefix + pump.repeat(base + bias) + ... + suffix`.
   */
  string: string;
  /**
   * An expression evaluating to the attack string in the host language of the dialect.
   *
   * - `javascript`: `'a'.repeat(31) + '\x00'`
   * - `python`: `'a' * 31 + '\x00'`, or `b'a' * 31 + b'\x00'` for `bytes`
   * - `ruby`: `"a" * 31 + "\x00"`, or `"a".b * 31 + "\x00".b` for `ASCII-8BIT`
   * - `java`: `"a".repeat(31) + "\u0000"`
   * - `dotnet`: `string.Concat(Enumerable.Repeat("a", 31)) + "\u0000"` (C#, `System.Linq`)
   * - `perl`: `"a" x 31 . "\x00"`
   * - `pcre`: `str_repeat('a', 31) . "\x00"` (PHP)
   */
  pattern: string;
};
