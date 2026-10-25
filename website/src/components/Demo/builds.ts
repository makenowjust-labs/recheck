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
export const FIRST_ESM_VERSION = "4.6.1";

type Version = [number, number, number];

const parseVersion = (text: string): Version | null => {
  const match = /^v?(\d+)\.(\d+)\.(\d+)$/.exec(text);
  return match === null
    ? null
    : [Number(match[1]), Number(match[2]), Number(match[3])];
};

const compareVersions = (a: Version, b: Version): number =>
  a[0] - b[0] || a[1] - b[1] || a[2] - b[2];

/** Stable release versions the Playground can load, newest first. */
export const releaseVersions = (candidates: readonly string[]): string[] => {
  const first = parseVersion(FIRST_ESM_VERSION)!;
  const versions = new Map<string, Version>();
  for (const candidate of candidates) {
    const version = parseVersion(candidate);
    if (version !== null && compareVersions(version, first) >= 0) {
      versions.set(version.join("."), version);
    }
  }
  return [...versions.entries()]
    .sort(([, a], [, b]) => compareVersions(b, a))
    .map(([text]) => text);
};

/** The build compiled into this site. */
export const bundledBuild = (customFields: Record<string, unknown>): Build => {
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

export const moduleUrl = (version: string): string =>
  `https://cdn.jsdelivr.net/npm/recheck@${version}/lib/browser.mjs`;

/** Finds the build shown with the given label in the version selector. */
export const findBuild = (
  builds: readonly Build[],
  label: string,
): Build | null => builds.find((build) => formatBuild(build) === label) ?? null;
