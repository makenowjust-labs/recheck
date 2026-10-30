module.exports = {
  presets: [require.resolve("@docusaurus/core/lib/babel/preset")],
  sourceType: "unambiguous",
  // The preset turns `import()` into `require()` in the server bundle, which cannot load a module from a URL.
  ignore: ["./src/components/Demo/importModule.js"],
};
