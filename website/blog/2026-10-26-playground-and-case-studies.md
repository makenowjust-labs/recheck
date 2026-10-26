---
title: Playground permalinks and Case Studies
description: The Playground now records a check in a permalink, and the new Case Studies page collects what recheck has been used for
authors:
  - makenowjust
---

The [Playground](/playground) now records a check in a permalink, so that a vulnerability report can cite it.
The website also has a new [Case Studies](/docs/case-studies) page, which collects the vulnerabilities found with recheck and the projects using it.
This post describes both, after a short note on the v4.6 releases.

<!-- truncate -->

## recheck v4.6

Before the main topics, a word on the releases.
v4.6.0 came out this month, more than a year and a half after v4.5.0, and v4.6.1 and v4.6.2 followed within two days.
Many changes had been waiting on the `main` branch for a long time, and the release was overdue.
Sorry for the wait.

The highlights are the ESLint flat config support in `eslint-plugin-redos`, ES modules in the `recheck` package, a Linux ARM64 native binary, and a fix to the fuzz checker that could report a linear regular expression as vulnerable when its attack string merely ran over `attackTimeout` ([#1865](https://github.com/makenowjust-labs/recheck/issues/1865)).
Under the hood, the sources moved to Scala 3 and the build to sbt 2.
See the CHANGELOG of [v4.6.0](https://github.com/makenowjust-labs/recheck/releases/tag/v4.6.0), [v4.6.1](https://github.com/makenowjust-labs/recheck/releases/tag/v4.6.1) and [v4.6.2](https://github.com/makenowjust-labs/recheck/releases/tag/v4.6.2) for the details.

## Playground permalinks

Until now, the Playground was a scratch pad.
You pasted a regular expression, read the result, and that was it.
When you wrote a vulnerability report, you had to copy the result into the report by hand, and a reader had no way to see what exactly had been checked.

Now each check has a permalink.
It records the input, the parameters, the result, and the version of recheck that produced it.
Open the link, and the Playground shows the recorded result, runs the check again with the same version, and tells whether the new result matches the recorded one.

The permalink is meant to be cited in a vulnerability report.
A reader of the report sees exactly what was checked, and reproduces it with one click, even after recheck changes.
The [path-to-regexp advisory](https://github.com/advisories/GHSA-27v5-c462-wpq7) links to the Playground to show its analysis, for example.
Such a link had only the input before, but now it carries the result as well.

To make the reproduction reliable, the Playground can run any release since v4.6.1, which is loaded from a CDN as ES modules, in addition to the build of the current `main` branch.
A permalink preselects the version it was recorded with.

## Case Studies

recheck has been around for six years, and the `recheck` package is downloaded millions of times a month.
Still, it was hard to answer what it has been used for, because the reports are scattered over advisories, blog posts and configuration files.
The [Case Studies](/docs/case-studies) page collects them in one place.

- **Vulnerabilities found with recheck.**
  From markdown-it and marked in 2022, to the thirteen CVEs in Ruby, Rack, Rails and other gems that [ooooooo_q](https://github.com/ooooooo-q) reported after checking the regular expressions of Ruby applications with recheck, to the findings of the recent [Revisiting ReDoS Attacks](https://mwlik.github.io/2026-04-15-revisiting-redos-attacks/) post.
- **Projects using recheck.**
  Zod tests every regular expression it ships, Civitai and pi-mcp-adapter guard regular expressions built at runtime, and core-js and markdownlint lint with `eslint-plugin-redos`.
  Gixy-Next checks nginx configurations through recheck, and a Datadog static analysis rule recommends it.
- **Tools built on recheck.**
  Command-line scanners, an HTTP API, and a Burp Suite extension.

The page is surely incomplete.
If recheck helped you to find a vulnerability or you use it in your project, please tell us in the [Reports](https://github.com/makenowjust-labs/recheck/discussions/categories/reports) discussion category.
An issue or a pull request that adds an entry to the page is welcome, too.

## Citing recheck

For the same reason, each release is now archived on Zenodo.
The concept DOI [10.5281/zenodo.23660268](https://doi.org/10.5281/zenodo.23660268) always resolves to the latest release, and the repository has a `CITATION.cff`, so that a paper or a report can cite recheck in a stable way.

## P.S.

If recheck is useful to you, consider supporting the project on [GitHub Sponsors](https://github.com/sponsors/makenowjust).
