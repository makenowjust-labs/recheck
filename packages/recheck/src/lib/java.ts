import { start as startAgent } from "./agent";
import * as exe from "./exe";

export async function ensure() {
  const jar = exe.jar();
  if (jar === null) {
    return null;
  }

  return await startAgent("java", ["-jar", jar, "agent"]);
}
