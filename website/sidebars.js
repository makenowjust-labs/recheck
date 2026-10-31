// @ts-check

/** @type {import('@docusaurus/plugin-content-docs').SidebarsConfig} */
const sidebars = {
  docs: [
    "intro",
    "case-studies",
    {
      type: "category",
      label: "Usage",
      link: {
        type: "generated-index",
      },
      collapsed: false,
      items: [
        "usage/as-javascript-library",
        "usage/as-scala-library",
        "usage/as-eslint-plugin",
        {
          type: "category",
          label: "Dialects",
          link: {
            type: "doc",
            id: "usage/dialects",
          },
          items: [
            "usage/dialects/javascript",
            "usage/dialects/python",
            "usage/dialects/ruby",
            "usage/dialects/java",
            "usage/dialects/dotnet",
            "usage/dialects/perl",
            "usage/dialects/pcre",
          ],
        },
        "usage/recall-validation",
        "usage/parameters",
        "usage/diagnostics",
      ],
    },
    {
      type: "category",
      label: "Internals",
      link: {
        type: "generated-index",
      },
      items: [
        "internals/background",
        "internals/fuzzing",
        "internals/matching-acceleration",
        "internals/criteria",
      ],
    },
  ],
};

module.exports = sidebars;
