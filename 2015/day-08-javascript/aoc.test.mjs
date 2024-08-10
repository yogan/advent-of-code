import test from "node:test"
import assert from "node:assert/strict"

import { variants, part1And2 } from "./aoc.mjs"

const sample = ['""', '"abc"', '"aaa\\"aaa"', '"\\x27"']
const expectedPart1 = ["", "abc", 'aaa"aaa', "'"]
const expectedPart2 = [
  '"\\"\\""',
  '"\\"abc\\""',
  '"\\"aaa\\\\\\"aaa\\""',
  '"\\"\\\\x27\\""',
]

test("variants gives the right part 1 results for the sample input", () => {
  assert.deepStrictEqual(
    sample.map(variants).map((v) => v[1]),
    expectedPart1,
  )
})

test("variants gives the right part 2 results for the sample input", () => {
  assert.deepStrictEqual(
    sample.map(variants).map((v) => v[2]),
    expectedPart2,
  )
})

test("part1And2 works for the sample input", () => {
  assert.deepEqual(part1And2(sample), [12, 19])
})
