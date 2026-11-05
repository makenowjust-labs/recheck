import type { Property, Setting } from "./index.js";

/** Returns the TypeScript type of the values that the property or the setting admits. */
export declare const generateTypescriptType: (
  property: Property | Setting,
) => string;
