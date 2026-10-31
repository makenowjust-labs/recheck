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

export type { Parameters } from "./parameters";

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
  /**
   * The recall validation of the attack string, or `null` if it is not run.
   */
  recallValidation: RecallValidation | null;
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
   * Whether the attack is effective on the engine.
   * The vulnerable diagnostics is accepted only if it is `true`.
   */
  succeeded: boolean;
};

/**
 * RecallResult is the result of matching the attack string on the real regex engine.
 *
 * - `finish`: The matching finished in `time` milliseconds, so the attack is not effective.
 * - `timeout`: The matching did not finish in time.
 * - `engine-error`: The engine gave up with a resource error, e.g. PHP's backtrack limit or Java's `StackOverflowError`.
 */
export type RecallResult =
  | { kind: "finish"; time: number }
  | { kind: "timeout" }
  | { kind: "engine-error"; message: string };

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
