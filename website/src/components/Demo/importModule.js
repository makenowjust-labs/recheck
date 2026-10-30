/**
 * @param {string} url
 * @returns {Promise<unknown>}
 */
export const importModule = (url) => import(/* webpackIgnore: true */ url);
