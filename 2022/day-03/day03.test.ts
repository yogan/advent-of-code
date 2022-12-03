import { test, expect } from "bun:test"
import { toPriority, splitIntoGroups, readLines, part1, part2 } from "./day03"

// we are using bun canary (0.3.0) which brings .toEqual (and other nice stuff),
// but bun-types are not available yet (still at 0.2.2), so we'll have to extend
// them a bit here.
declare module "bun:test" {
    interface Expect {
        toEqual(value: any): void
    }
}

test("toPriority", () => {
    expect(toPriority("a")).toBe(1)
    expect(toPriority("z")).toBe(26)
    expect(toPriority("A")).toBe(27)
    expect(toPriority("Z")).toBe(52)
})

test("splitIntoGroups", () => {
    const groups = splitIntoGroups([
        "a1",
        "a2",
        "a3",
        "b1",
        "b2",
        "b3",
        "c1",
        "c2",
        "c3",
    ])

    expect(groups).toEqual([
        ["a1", "a2", "a3"],
        ["b1", "b2", "b3"],
        ["c1", "c2", "c3"],
    ])
})

const lines = readLines("./day03.sample")

test("day 3 part 1 works", () => {
    expect(part1(lines)).toBe(157)
})

test("day 3 part 2 works", () => {
    expect(part2(lines)).toBe(70)
})
