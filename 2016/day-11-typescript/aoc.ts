export type Chip = { type: 'chip'; element: string }
export type Generator = { type: 'generator'; element: string }
export type Item = Chip | Generator
export type State = { elevator: number; floors: Set<Item>[] }

export async function main() {
    const filename = process.argv[2] || 'input.txt'
    const initialState = await parseInput(filename)
    console.log('Part 1:', part1(initialState))
}

async function parseInput(filename: string): Promise<State> {
    const content = await Bun.file(filename).text()
    const floors = content.split('\n').filter(Boolean).map(parseLine)
    return { elevator: 0, floors }
}

export function parseLine(line: string): Set<Item> {
    const shorten = (name: string) => name.slice(0, 2).toUpperCase()

    const toItem = (desc: string): Item =>
        desc.endsWith('generator')
            ? { type: 'generator', element: shorten(desc) }
            : { type: 'chip', element: shorten(desc) }

    return line.includes('nothing relevant')
        ? new Set()
        : line
              .replace(/The \w+ floor contains /, '')
              .replace(/\.$/, '')
              .split(/,? and |, /)
              .map((item) => item.replace(/a |an /, ''))
              .reduce((acc, item) => acc.add(toItem(item)), new Set<Item>())
}

export function nextStates(state: State): State[] {
    const states: State[] = []

    if (state.elevator < 3) {
        // move one floor up
        states.push(...findNextStates(state, state.elevator + 1))
    }

    if (state.elevator > 0) {
        // move one floor down
        states.push(...findNextStates(state, state.elevator - 1))
    }

    return states
}

function findNextStates(state: State, target: number): State[] {
    const currentFloorItems = [...state.floors[state.elevator]]
    const targetFloorItems = [...state.floors[target]]

    const candidates = combinations(currentFloorItems, 1).concat(
        combinations(currentFloorItems, 2)
    )

    const safeCandidates = candidates.filter((candidate) =>
        isSafe([...candidate, ...targetFloorItems])
    )

    return safeCandidates.map((candidate) => {
        const itemsOfCurrentFloor = new Set(
            currentFloorItems.filter((item) => !candidate.includes(item))
        )

        const itemsOfTargetFloor = new Set([
            ...state.floors[target],
            ...candidate,
        ])

        // going down
        if (state.elevator > target) {
            return {
                elevator: target,
                floors: [
                    ...state.floors.slice(0, target),
                    itemsOfTargetFloor,
                    itemsOfCurrentFloor,
                    ...state.floors.slice(state.elevator + 1),
                ],
            }
        }

        // going up
        return {
            elevator: target,
            floors: [
                ...state.floors.slice(0, state.elevator),
                itemsOfCurrentFloor,
                itemsOfTargetFloor,
                ...state.floors.slice(target + 1),
            ],
        }
    })
}

export function isSafe(items: Item[]): boolean {
    const chips = items.filter((item) => item.type === 'chip')
    const generators = items.filter((item) => item.type === 'generator')

    return chips.every((chip) => {
        if (generators.find((gen) => gen.element === chip.element)) {
            // chip is protected by its matching generator
            return true
        }

        // no matching generator found, so if there are _any_ generators,
        // the _will_ be dangerous and fry the chip
        return generators.length === 0
    })
}

export function combinations<T>(items: T[], count: number): T[][] {
    if (count === 0) {
        return [[]]
    }

    if (items.length === 0) {
        return []
    }

    const [first, ...rest] = items
    const withFirst = combinations(rest, count - 1).map((c) => [first, ...c])
    const withoutFirst = combinations(rest, count)
    return [...withFirst, ...withoutFirst]
}

export function serialize(state: State): string {
    const serializeItem = (item: Item) =>
        item.type === 'chip' ? item.element + '-C' : item.element + '-G'

    let result = String(state.elevator)

    for (const items of state.floors) {
        const hashItems = [...items].map(serializeItem).sort().join(',')
        result += `|${hashItems}`
    }

    return result
}

export function isFinalState(state: State): boolean {
    return (
        state.elevator === 3 &&
        state.floors.slice(0, 3).every((f) => f.size === 0)
    )
}

export function part1(initialState: State): number {
    const seen = new Set<string>([serialize(initialState)])
    const queue = [{ steps: 0, state: initialState }]

    let minSteps = Infinity

    while (queue.length > 0) {
        const { steps, state } = queue.shift()!

        for (const next of nextStates(state)) {
            const serialized = serialize(next)
            if (seen.has(serialized)) {
                continue
            }
            seen.add(serialized)

            if (isFinalState(next)) {
                minSteps = Math.min(minSteps, steps + 1)
            }

            queue.push({ steps: steps + 1, state: next })
        }
    }

    return minSteps
}
