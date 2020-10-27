/**
 * Checks the given RegExp pattern `source` and `flags` is ReDoS vulnerable.
 * And, it takes an optional argument to specify `timeout` milliseconds.
 */
export function check(source: string, flags: string, timeout?: number): Complexity;

/**
 * Complexity is an analysis result.
 * It is a matching time complexity of the RegExp pattern.
 * `"constant"` and `"linear"` are safe, but `"polynomial"` and `"exponential"`
 * are unsafe potentially.
 */
export type Complexity =
  | { complexity: "constant" | "linear" }
  | {
      complexity: "polynomial";
      degree: number;
      witness: Witness;
    }
  | {
      complexity: "exponential";
      witness: Witness;
    };

/**
 * Witness is an witness of ReDoS vulnerability.
 * In short, it is an attack string pattern.
 */
export type Witness = {
  pumps: {
    prefix: string;
    pump: string;
  }[];
  suffix: string;
};
