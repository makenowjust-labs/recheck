// @ts-check

const math = require('remark-math');
const katex = require('rehype-katex');
const lightCodeTheme = require('prism-react-renderer/themes/github');
const darkCodeTheme = require('prism-react-renderer/themes/dracula');

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: 'recheck',
  tagline: 'The trustworthy ReDoS checker',
  url: 'https://makenowjust-labo.github.io',
  baseUrl: '/recheck/',
  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',
  favicon: 'img/favicon.png',
  organizationName: 'MakeNowJust-Labo',
  projectName: 'recheck',

  presets: [
    [
      'classic',
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          sidebarPath: require.resolve('./sidebars.js'),
          editUrl: 'https://github.com/MakeNowJust-Labo/recheck/tree/main/website/docs',
          remarkPlugins: [math],
          rehypePlugins: [katex],
        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
      }),
    ],
  ],

  plugins: ['@docusaurus/plugin-ideal-image'],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      navbar: {
        title: 'Recheck',
        logo: {
          alt: 'Recheck Logo',
          src: 'img/logo-light.svg',
          srcDark: 'img/logo-dark.svg',
        },
        items: [
          {
            type: 'doc',
            docId: 'intro',
            position: 'left',
            label: 'Docs',
          },
          {
            to: 'playground',
            position: 'left',
            label: 'Playground',
          },
          {
            href: 'https://github.com/MakeNowJust-Labo/recheck',
            label: 'GitHub',
            position: 'right',
          },
        ],
      },
      footer: {
        style: 'dark',
        links: [
          {
            title: 'Docs',
            items: [
              {
                label: 'Introduction',
                to: '/docs/intro',
              },
              {
                label: 'Usage',
                to: '/docs/category/usage',
              },
              {
                label: 'Internals',
                to: '/docs/category/internals',
              },
            ],
          },
          {
            title: 'Try',
            items: [
              {
                label: 'Playground',
                to: '/playground',
              },
            ],
          },
          {
            title: 'More',
            items: [
              {
                label: 'GitHub Repository',
                href: 'https://github.com/MakeNowJust-Labo/recheck',
              },
              {
                label: '@MakeNowJust-Labo',
                href: 'https://github.com/MakeNowJust-Labo',
              },
            ],
          },
        ],
        copyright: `Copyright © 2020-2022 TSUYUSATO "MakeNowJust" Kitsune. Built with Docusaurus.`,
      },
      prism: {
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme,
        additionalLanguages: ['java', 'scala'],
      },
    }),

  stylesheets: [
    {
      href: 'https://cdn.jsdelivr.net/npm/katex@0.13.11/dist/katex.min.css',
      integrity:
        'sha384-Um5gpz1odJg5Z4HAmzPtgZKdTBHZdw8S29IecapCSB31ligYPhHQZMIlWLYQGVoc',
      crossorigin: 'anonymous',
    },
  ],
};

module.exports = config;
