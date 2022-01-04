module.exports = {
  roots: ["<rootDir>/src"],
  testMatch: ["**/?(*.)+(spec|test).+(js|ts|tsx)"],
  transform: {
    "^.+\\.(js|ts|tsx)$": ["esbuild-jest", { sourcemap: true }],
  },
  coverageProvider: "v8",
};
