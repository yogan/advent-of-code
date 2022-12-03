import { expect, test } from "bun:test"
import { toPriority, part1 } from "./day03"

test("toPriority", () => {
    expect(toPriority("a")).toBe(1)
    expect(toPriority("z")).toBe(26)
    expect(toPriority("A")).toBe(27)
    expect(toPriority("Z")).toBe(52)
})

test("day 3 part 1 works", () => {
    expect(part1("day03.sample")).toBe(157)
})
