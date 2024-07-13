type Dimensions = readonly [number, number, number]

export async function main() {
    const filename = process.argv[2] || 'input.txt'
    const boxes = await parseInput(filename)
    console.log('Part 1:', part1(boxes))
    console.log('Part 2:', part2(boxes))
}

export async function parseInput(filename: string): Promise<Dimensions[]> {
    const content = await Bun.file(filename).text()
    return content.split('\n').filter(Boolean).map(parseLine)
}

export function parseLine(line: string): Dimensions {
    const [l, w, h] = line.split('x').map(Number)
    return [l, w, h]
}

export function part1(boxes: Dimensions[]): number {
    return boxes.reduce((acc, box) => acc + volume(box), 0)
}

export function part2(boxes: Dimensions[]): number {
    return boxes.reduce((acc, box) => acc + surfaceArea(box), 0)
}

function surfaceArea([l, w, h]: Dimensions): number {
    return 2 * (l * w + w * h + h * l)
}

function volume([l, w, h]: Dimensions): number {
    return l * w * h
}
