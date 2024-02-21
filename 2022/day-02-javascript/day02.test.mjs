import test from "node:test"
import assert from "node:assert/strict"

import { part1, part2 } from "./day02.mjs"

test("correct sample score for part 1", async (t) => {
  const score = await part1("./day02.sample")
  assert.strictEqual(score, 15)
})

test("correct sample score for part 2", async (t) => {
  const score = await part2("./day02.sample")
  assert.strictEqual(score, 12)
})

test("correct real input score for part 1", async (t) => {
  const score = await part1("./day02.in")
  assert.strictEqual(score, 17189)
})

test("correct real input score for part 2", async (t) => {
  const score = await part2("./day02.in")
  assert.strictEqual(score, 13490)
})
