/**
 * Checks the given RegExp pattern `source` and `flags` is ReDoS vulnerable.
 * And, it takes an optional argument to specify `timeout` milliseconds.
 */
export function check(source: string, flags: string, timeout?: number): Diagnostics;

/**
 * Diagnostics is an analyzing result.
 * It takes one of the following three statuses:
 *
 * - `safe`: The given RegExp is safe.
 * - `vulnerable`: The given RegExp is potentially vulnerable.
 * - `unknown`: An error is occured on analyzing.
 *   As a result, it is unknown wherher the RegExp is safe or vulnerable.
 */
export type Diagnostics =
  | {
      status: "safe";
      complexity?: { type: "constant" | "linear" };
    }
  | {
      status: "vulnerable";
      attack: string;
      complexity?:
        | {
            type: "exponential";
            witness: Witness;
          }
        | {
            type: "polynomial";
            degree: number;
            witness: Witness;
          };
    }
  | {
      status: "unknown";
      error:
        | { kind: "timeout" }
        | {
            kind: "unsupported" | "invalid";
            message: string;
          };
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
