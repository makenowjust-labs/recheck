import type { Property, Setting, Variant } from "./index.js";

/** Returns the TypeScript type of the parameter, or of an alternative or a setting of its `oneOf`. */
export declare const typescriptType: (
  property: Property | Variant | Setting,
) => string;
