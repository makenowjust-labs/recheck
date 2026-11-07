/** Identifies the `recheck` build that produced a result. */
export type Build =
  | { type: "release"; version: string }
  | { type: "head"; commit: string }
  | { type: "dev" };

export const formatBuild = (build: Build): string => {
  switch (build.type) {
    case "release":
      return `v${build.version}`;
    case "head":
      return `HEAD (${build.commit.slice(0, 7)})`;
    case "dev":
      return "development build";
  }
};

export const sameBuild = (a: Build, b: Build): boolean =>
  formatBuild(a) === formatBuild(b);

/** The first release that ships `lib/browser.mjs`. */
const FIRST_ESM_VERSION = "4.6.1";

/** Version is a release version, whose prerelease is the label and the number of e.g. `-beta.1`, or `null`. */
type Version = {
  core: [number, number, number];
  prerelease: [string, number] | null;
};

const parseVersion = (text: string): Version | null => {
  const match = /^v?(\d+)\.(\d+)\.(\d+)(?:-([a-z]+)\.(\d+))?$/.exec(text);
  if (match === null) return null;
  return {
    core: [Number(match[1]), Number(match[2]), Number(match[3])],
    prerelease: match[4] === undefined ? null : [match[4], Number(match[5])],
  };
};

const formatVersion = ({ core, prerelease }: Version): string =>
  core.join(".") + (prerelease === null ? "" : `-${prerelease.join(".")}`);

const compareCores = (a: Version, b: Version): number =>
  a.core[0] - b.core[0] || a.core[1] - b.core[1] || a.core[2] - b.core[2];

/** Compares the versions as semver does, where a prerelease precedes its release. */
const compareVersions = (a: Version, b: Version): number => {
  const core = compareCores(a, b);
  if (core !== 0) return core;
  if (a.prerelease === null || b.prerelease === null) {
    return Number(a.prerelease === null) - Number(b.prerelease === null);
  }
  return (
    a.prerelease[0].localeCompare(b.prerelease[0]) ||
    a.prerelease[1] - b.prerelease[1]
  );
};

/** Checks whether the build has the changes of the release version, as a later release or an unreleased build.
 *
 * A prerelease counts as its release (e.g. `5.0.0-beta.1` has the changes of `5.0.0`).
 */
export const isAtLeast = (build: Build, version: string): boolean => {
  if (build.type !== "release") return true;
  const a = parseVersion(build.version);
  const b = parseVersion(version);
  return a === null || b === null || compareCores(a, b) >= 0;
};

/** Release versions the Playground can load, newest first.
 *
 * They are the stable releases from the first one with `lib/browser.mjs`, and the prereleases newer than all of them.
 */
const releaseVersions = (candidates: readonly string[]): string[] => {
  const first = parseVersion(FIRST_ESM_VERSION)!;
  const versions = new Map<string, Version>();
  for (const candidate of candidates) {
    const version = parseVersion(candidate);
    if (version !== null && compareVersions(version, first) >= 0) {
      versions.set(formatVersion(version), version);
    }
  }
  const sorted = [...versions.values()].sort((a, b) => compareVersions(b, a));
  const stable = sorted.find(({ prerelease }) => prerelease === null);
  return sorted
    .filter(
      (version) =>
        version.prerelease === null ||
        stable === undefined ||
        compareVersions(version, stable) > 0,
    )
    .map(formatVersion);
};

/** The build compiled into this site. */
const bundledBuild = (customFields: Record<string, unknown>): Build => {
  const commit = customFields["commit"];
  return typeof commit === "string" && commit !== ""
    ? { type: "head", commit }
    : { type: "dev" };
};

/** Releases first, newest to oldest, then the bundled build. */
export const availableBuilds = (
  customFields: Record<string, unknown>,
): Build[] => {
  const tags = customFields["tags"];
  const candidates = [
    ...(Array.isArray(tags) ? tags : []),
    customFields["latestVersion"],
  ].filter((value): value is string => typeof value === "string");
  return [
    ...releaseVersions(candidates).map((version): Build => ({
      type: "release",
      version,
    })),
    bundledBuild(customFields),
  ];
};

/** Finds the build shown with the given label in the version selector. */
export const findBuild = (
  builds: readonly Build[],
  label: string,
): Build | null => builds.find((build) => formatBuild(build) === label) ?? null;
