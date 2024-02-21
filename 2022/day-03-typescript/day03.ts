import { readFileSync } from "node:fs"

export const readLines = (filename: PathOrFileDescriptor) =>
    readFileSync(filename, { encoding: "utf-8" })
        .split("\n")
        .filter((line) => line.trim().length > 0)

const intersection = <T>(s1: Set<T>, s2: Set<T>) =>
    new Set(Array.from(s1).filter((x) => s2.has(x)))

const intersection3 = <T>(s1: Set<T>, s2: Set<T>, s3: Set<T>) =>
    intersection(intersection(s1, s2), s3)

const extractFromSet = <T>(set: Set<T>): T => Array.from(set)[0]

export const toPriority = (letter: string) => {
    const offsetLowercase = "a".charCodeAt(0) - 1
    const offsetUppercase = "A".charCodeAt(0) - 1 - 26
    const ascii = letter.charCodeAt(0)

    return ascii > offsetLowercase
        ? ascii - offsetLowercase
        : ascii - offsetUppercase
}

const splitIntoCompartments = (rucksack: string) => {
    const middle = rucksack.length / 2
    return [
        new Set(rucksack.slice(0, middle)),
        new Set(rucksack.slice(middle)),
    ] as const
}

type Group<T> = [T, T, T]

export const splitIntoGroups = <T>(lines: T[]): Group<T>[] => {
    function splitRec(list: T[], out: Group<T>[]): void {
        if (list.length === 0) {
            return
        }

        const [a, b, c, ...rest] = list
        out.push([a, b, c])

        splitRec(rest, out)
    }

    const groups: Group<T>[] = []
    splitRec(lines, groups)
    return groups
}

export const part1 = (lines: string[]) =>
    lines
        .map(splitIntoCompartments)
        .map(([left, right]) => intersection(left, right))
        .map(extractFromSet)
        .map(toPriority)
        .reduce((a, b) => a + b, 0)

export const part2 = (lines: string[]) =>
    splitIntoGroups(lines)
        .map(([one, two, three]) =>
            intersection3(new Set(one), new Set(two), new Set(three))
        )
        .map(extractFromSet)
        .map(toPriority)
        .reduce((a, b) => a + b, 0)

const lines = readLines("./day03.in")
console.log(`Day 02 part 1: ${part1(lines)}`)
console.log(`Day 02 part 2: ${part2(lines)}`)
