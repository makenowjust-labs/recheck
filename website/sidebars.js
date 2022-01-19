// @ts-check

/** @type {import('@docusaurus/plugin-content-docs').SidebarsConfig} */
const sidebars = {
  docs: [
    'intro',
    {
      type: 'category',
      label: 'Usage',
      link: {
        type: 'generated-index',
      },
      collapsed: false,
      items: [
        'usage/as-javascript-library',
        'usage/as-scala-library',
        'usage/as-eslint-plugin',
        'usage/parameters',
        'usage/diagnostics'
      ],
    },
    {
      type: 'category',
      label: 'Internals',
      link: {
        type: 'generated-index',
      },
      items: [
        'internals/background',
        'internals/fuzzing',
        'internals/matching-acceleration',
        'internals/criteria',
      ]
    }
  ]
};

module.exports = sidebars;
