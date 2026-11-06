import { build } from "esbuild";

import type { Plugin, PluginBuild, OnResolveArgs } from "esbuild";

// See https://github.com/evanw/esbuild/issues/619#issuecomment-751995294.
// The private `recheck-parameters` is bundled because it is not published.
const makeAllPackagesExternalPlugin: Plugin = {
  name: "make-all-packages-external",
  setup(build: PluginBuild) {
    const filter = /^[^.\/]|^\.[^.\/]|^\.\.[^\/]/; // Must not start with "/" or "./" or "../"
    build.onResolve({ filter }, (args: OnResolveArgs) =>
      args.path === "recheck-parameters"
        ? undefined
        : { path: args.path, external: true },
    );
    // `package.json` is read at run time, so the plugin reports the installed version.
    // `lib/main.js` and `src/main.ts` are at the same depth, so `../package.json` points to the same file.
    build.onResolve(
      { filter: /^\.\.\/package\.json$/ },
      (args: OnResolveArgs) => ({
        path: args.path,
        external: true,
      }),
    );
  },
};

const main = async () => {
  await build({
    entryPoints: ["src/main.ts"],
    bundle: true,
    format: "cjs",
    target: "es2020",
    platform: "node",
    plugins: [makeAllPackagesExternalPlugin],
    outfile: "lib/main.js",
    // `require("eslint-plugin-redos")` must give the plugin itself, not `{ default }`.
    footer: { js: "module.exports = module.exports.default;" },
  });
};

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
