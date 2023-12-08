type Dimensions = readonly [number, number, number]

function surfaceArea([l, w, h]: Dimensions): number {
    return 2 * (l * w + w * h + h * l)
}

function volume([l, w, h]: Dimensions): number {
    return l * w * h
}

export function part1(boxes: Dimensions[]): number {
    return boxes.reduce((acc, box) => acc + volume(box), 0)
}

export function part2(boxes: Dimensions[]): number {
    return boxes.reduce((acc, box) => acc + surfaceArea(box), 0)
}

export function parseLine(line: string): Dimensions {
    const [l, w, h] = line.split("x").map(Number)
    return [l, w, h]
}

export async function parseInput(): Promise<Dimensions[]> {
    const file = Bun.file("input.txt")
    const text = await file.text()
    return text.split("\n").map(parseLine)
}

export async function main() {
    const boxes = await parseInput()
    console.log("Part 1:", part1(boxes))
    console.log("Part 2:", part2(boxes))
}
