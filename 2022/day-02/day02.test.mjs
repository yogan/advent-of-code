import test from "node:test"
import assert from "node:assert/strict"

import { day02 } from "./day02.mjs"

test("day 02 sample returns correct score", async (t) => {
  const score = await day02("./day02.sample")
  assert.strictEqual(score, 15)
})
