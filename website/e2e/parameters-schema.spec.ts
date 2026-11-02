import { expect, test } from "@playwright/test";
import { schema } from "recheck-parameters";

test("the schema of the parameters is published at its $id", async ({
  request,
}) => {
  const response = await request.get(new URL(schema.$id).pathname);
  expect(response.ok()).toBe(true);
  expect(await response.json()).toEqual(schema);
});
