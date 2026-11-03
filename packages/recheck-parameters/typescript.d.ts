import type { Property, Setting, Variant } from "./index.js";

/** Returns the TypeScript type of the values that the property, the variant or the setting admits. */
export declare const generateTypescriptType: (
  property: Property | Variant | Setting,
) => string;
