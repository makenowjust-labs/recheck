import { start as startAgent } from "./agent";
import * as exe from "./exe";

export async function ensure() {
  const bin = exe.bin();
  if (bin === null) {
    return null;
  }

  return await startAgent(bin, ["agent"]);
}
