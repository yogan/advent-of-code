import { readFileSync } from "node:fs"

const readLines = (filename: PathOrFileDescriptor) =>
    readFileSync(filename, { encoding: "utf-8" })
        .split("\n")
        .filter((line) => line.trim().length > 0)

const intersection = <T>(s1: Set<T>, s2: Set<T>) =>
    new Set(Array.from(s1).filter((x) => s2.has(x)))

const extractFromSet = <T>(set: Set<T>): T => Array.from(set)[0]

export const toPriority = (letter: string) => {
    const offsetLowercase = "a".charCodeAt(0) - 1
    const offsetUppercase = "A".charCodeAt(0) - 1 - 26
    const ascii = letter.charCodeAt(0)

    return ascii > offsetLowercase
        ? ascii - offsetLowercase
        : ascii - offsetUppercase
}

const splitRucksack = (rucksack: string) => {
    const middle = rucksack.length / 2
    return [
        new Set(rucksack.slice(0, middle)),
        new Set(rucksack.slice(middle)),
    ] as const
}

export const part1 = (filename: string) =>
    readLines(filename)
        .map(splitRucksack)
        .map(([left, right]) => intersection(left, right))
        .map(extractFromSet)
        .map(toPriority)
        .reduce((a, b) => a + b, 0)

console.log(`Day 02 part 1: ${part1("./day03.in")}`)
