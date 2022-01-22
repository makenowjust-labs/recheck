import { build as esbuild } from "esbuild";

import type {
  BuildOptions,
  Plugin,
  PluginBuild,
  OnLoadArgs,
  OnResolveArgs,
} from "esbuild";

const isProduction = process.env["NODE_ENV"] === "production";

// See https://github.com/evanw/esbuild/issues/619#issuecomment-751995294.
const makeAllPackagesExternalPlugin: Plugin = {
  name: "make-all-packages-external",
  setup(build: PluginBuild) {
    const filter = /^[^./]|^\.[^./]|^\.\.[^/]/; // Must not start with "/" or "./" or "../"
    build.onResolve({ filter }, (args: OnResolveArgs) => ({
      path: args.path,
      external: true,
    }));
  },
};

const inlineWorkerPlugin: Plugin = {
  name: "inline-worker",
  setup(build: PluginBuild) {
    const { plugins, ...initialOptions } = build.initialOptions;
    const filter = /[./]worker\.(?:js|ts)$/;
    build.onLoad({ filter }, async (args: OnLoadArgs) => {
      const result = await esbuild({
        ...initialOptions,
        entryPoints: [args.path],
        inject: [`src/inject/worker-${initialOptions.platform}.ts`],
        format: "iife",
        write: false,
        plugins: plugins?.filter((plugin) => plugin !== inlineWorkerPlugin),
        sourcemap: "inline",
      });
      const workerCode = result.outputFiles![0].text;

      let contents = "";
      contents += `const script = ${JSON.stringify(workerCode)};\n`;
      contents += "\n";
      contents += "export default function createInlineWorker() {\n";
      contents +=
        '  const blob = new Blob([script], { type: "text/javascript" });\n';
      contents += "  const url = URL.createObjectURL(blob);\n";
      contents += "  const worker = new Worker(url);\n";
      contents += "  URL.revokeObjectURL(url);\n";
      contents += "  return worker;\n";
      contents += "}\n";

      return {
        contents,
        loader: "js",
      };
    });
  },
};

const main = async () => {
  await esbuild({
    entryPoints: ["src/main.ts"],
    inject: ["src/inject/main.ts"],
    bundle: true,
    minify: isProduction,
    format: "cjs",
    logLevel: "error",
    platform: "node",
    plugins: [makeAllPackagesExternalPlugin, inlineWorkerPlugin],
    outfile: "lib/main.js",
  });
  await esbuild({
    entryPoints: ["src/browser.ts"],
    bundle: true,
    minify: isProduction,
    format: "cjs",
    logLevel: "error",
    platform: "browser",
    plugins: [makeAllPackagesExternalPlugin, inlineWorkerPlugin],
    outfile: "lib/browser.js",
  });
};

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
