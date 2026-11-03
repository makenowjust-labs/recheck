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
 * Diagnostics is what `check` reports about a RegExp: the evidence for judging its ReDoS risk, not a final verdict.
 * It takes one of the following three statuses:
 *
 * - `safe`: The checking found no attack string within its limits.
 * - `vulnerable`: The checking found an attack string.
 * - `unknown`: The checking could not finish because of an error, so it gives no evidence either way.
 */
export type Diagnostics =
  SafeDiagnostics | VulnerableDiagnostics | UnknownDiagnostics;

/**
 * SafeDiagnostics is the type of the diagnostics in which the checking found no attack string.
 */
export type SafeDiagnostics = {
  source: string;
  flags: string;
  dialect: Dialect;
  unitMode: UnitMode;
  status: "safe";
  complexity: SafeComplexity;
  refutedAttacks: RefutedAttack[];
  checker: "automaton" | "fuzz";
};

/**
 * VulnerableDiagnostics is the type of the diagnostics with the attack string that the checking found.
 */
export type VulnerableDiagnostics = {
  source: string;
  flags: string;
  dialect: Dialect;
  unitMode: UnitMode;
  status: "vulnerable";
  complexity: VulnerableComplexity;
  attack: AttackPattern;
  hotspot: Hotspot[];
  /**
   * The run of the attack string on the real regex engine, or `null` if it is not run.
   */
  runtimeAttack: RuntimeAttack | null;
  refutedAttacks: RefutedAttack[];
  checker: "automaton" | "fuzz";
};

/**
 * UnitMode is the unit of the attack strings: UTF-16 code units (`utf16`), code points (`code-point`), or bytes (`byte`).
 */
export type UnitMode = "utf16" | "code-point" | "byte";

/**
 * RefutedAttack is an attack that the runtime-attack verification refuted before the result.
 */
export type RefutedAttack = {
  complexity: VulnerableComplexity;
  attack: AttackPattern;
  hotspot: Hotspot[];
  runtimeAttack: RuntimeAttack;
};

/**
 * RuntimeAttack is a run of the attack string on the real regex engine of the dialect.
 */
export type RuntimeAttack = {
  /**
   * The runtime name and version, e.g. `"node v26.11.1"`.
   */
  runtime: string;
  /**
   * The timeout of the matching in milliseconds, or `null` if it is infinite.
   */
  timeout: number | null;
  result: RuntimeAttackResult;
  /**
   * Whether the engine confirms the attack, i.e. the attack is effective there (`timeout` or `engine-limit`).
   * Otherwise, the engine refutes the attack.
   */
  isConfirmed: boolean;
};

/**
 * RuntimeAttackResult is the result of matching the attack string on the real regex engine.
 *
 * - `finish`: The matching finished in `time` milliseconds, so the attack is not effective.
 * - `timeout`: The matching did not finish in time.
 * - `engine-limit`: The engine stopped on its limit of the matching steps, e.g. PHP's backtrack limit, so the attack is effective.
 * - `engine-error`: The engine gave up with an error on the stack depth or the memory, e.g. Java's `StackOverflowError`.
 *   It may happen on a long string even if the matching takes linear time, so the attack is not considered effective.
 */
export type RuntimeAttackResult =
  | { kind: "finish"; time: number }
  | { kind: "timeout" }
  | { kind: "engine-limit"; message: string }
  | { kind: "engine-error"; message: string };

/**
 * UnknownDiagnostics is the type of the diagnostics of a checking that could not finish because of an error.
 */
export type UnknownDiagnostics = {
  source: string;
  flags: string;
  dialect: Dialect;
  /**
   * The unit mode of the pattern, or `null` if the error happened on parsing the pattern.
   */
  unitMode: UnitMode | null;
  status: "unknown";
  error: Error;
  refutedAttacks: RefutedAttack[];
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
  Timeout | Cancel | Unsupported | Invalid | RuntimeAttacksRefuted | Unexpected;

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
 * RuntimeAttacksRefuted is an error reported when the runtime-attack verification refutes `maxRefutedAttacks` attacks.
 */
export type RuntimeAttacksRefuted = { kind: "runtime-attacks-refuted" };

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
