import type { Property, Setting, Variant } from "./index.js";

export declare const generateTypescriptType: (
  property: Property | Variant | Setting,
) => string;
