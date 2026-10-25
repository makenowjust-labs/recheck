import { availableBuilds, findBuild, formatBuild, sameBuild } from "./builds";

import type { Build } from "./builds";

const build: Build = { type: "release", version: "4.6.1" };

describe("formatBuild and sameBuild", () => {
  test("formats each kind of build", () => {
    expect(formatBuild(build)).toBe("v4.6.1");
    expect(
      formatBuild({ type: "head", commit: "0123456789abcdef0123456789abcdef" }),
    ).toBe("HEAD (0123456)");
    expect(formatBuild({ type: "dev" })).toBe("development build");
  });

  test("compares builds by their identity", () => {
    expect(sameBuild(build, { type: "release", version: "4.6.1" })).toBe(true);
    expect(sameBuild(build, { type: "release", version: "4.6.0" })).toBe(false);
    expect(sameBuild(build, { type: "dev" })).toBe(false);
    expect(
      sameBuild(
        { type: "head", commit: "abc" },
        { type: "head", commit: "abd" },
      ),
    ).toBe(false);
  });
});

describe("availableBuilds", () => {
  test("lists the releases from the tags and the latest version, then the bundled build", () => {
    expect(
      availableBuilds({
        tags: ["v4.6.0", "v4.6.1"],
        latestVersion: "4.6.2",
        commit: "0123456789",
      }),
    ).toEqual([
      { type: "release", version: "4.6.2" },
      { type: "release", version: "4.6.1" },
      { type: "head", commit: "0123456789" },
    ]);
  });

  test("keeps stable releases from 4.6.1 on, newest first", () => {
    expect(
      availableBuilds({
        tags: [
          "v4.5.0",
          "v4.6.0",
          "v4.6.1",
          "v4.10.0",
          "v4.7.0-beta.1",
          "v5.0.0",
        ],
      }),
    ).toEqual([
      { type: "release", version: "5.0.0" },
      { type: "release", version: "4.10.0" },
      { type: "release", version: "4.6.1" },
      { type: "dev" },
    ]);
  });

  test("accepts versions without the prefix and drops duplicates and junk", () => {
    expect(
      availableBuilds({
        tags: ["v4.6.1", "", "head"],
        latestVersion: "4.6.1",
        commit: "",
      }),
    ).toEqual([{ type: "release", version: "4.6.1" }, { type: "dev" }]);
  });

  test("falls back to the bundled build alone", () => {
    expect(availableBuilds({ latestVersion: "0.0.0-dev" })).toEqual([
      { type: "dev" },
    ]);
  });
});

describe("findBuild", () => {
  const builds = availableBuilds({ tags: ["v4.6.1"], commit: "0123456789" });

  test("maps a selector label back to its build", () => {
    expect(findBuild(builds, "v4.6.1")).toEqual({
      type: "release",
      version: "4.6.1",
    });
    expect(findBuild(builds, "HEAD (0123456)")).toEqual({
      type: "head",
      commit: "0123456789",
    });
    expect(findBuild(builds, "v4.6.0")).toBeNull();
  });
});
