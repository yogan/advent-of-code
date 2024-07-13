import { expect, test } from "bun:test"
import { parseLine, parseInput, part1, part2 } from "./aoc"

test("parseLine should split the line by 'x'", () => {
    expect(parseLine("1x2x420")).toEqual([1, 2, 420])
})

test("parseInput should parse the input file", async () => {
    const boxes = await parseInput("input.txt")

    expect(boxes).toHaveLength(5)
    expect(boxes[0]).toEqual([1, 1, 1])
    expect(boxes[4]).toEqual([11, 4, 11])
})

test("part1 should return the sum of the volumes of the boxes", () => {
    const b1 = [2, 3, 4] as const
    const b2 = [1, 1, 10] as const
    const volume1 = 2 * 3 * 4
    const volume2 = 1 * 1 * 10
    const expected = volume1 + volume2

    expect(part1([b1, b2])).toEqual(expected)
})

test("part2 should return the sum of the surface areas of the boxes", () => {
    const b1 = [2, 3, 4] as const
    const b2 = [1, 1, 10] as const
    const surface1 = 2 * (2 * 3 + 3 * 4 + 4 * 2)
    const surface2 = 2 * (1 * 1 + 1 * 10 + 10 * 1)
    const expected = surface1 + surface2

    expect(part2([b1, b2])).toEqual(expected)
})
