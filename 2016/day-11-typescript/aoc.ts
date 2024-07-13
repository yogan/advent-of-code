export type Chip = { type: 'chip'; element: string }
export type Generator = { type: 'generator'; element: string }
export type Item = Chip | Generator
export type State = { elevator: number; floors: Set<Item>[] }

export async function main() {
    const filename = process.argv[2] || 'input.txt'
    const initialState = await parseInput(filename)

    console.log('Part 1:', simulate(initialState))
    console.log('Part 2:', simulate(addPart2ItemsToGroundFloor(initialState)))
}

export function simulate(initialState: State): number {
    const seen = new Set([serialize(initialState)])
    const queue = [{ steps: 0, state: initialState }]

    while (queue.length > 0) {
        const { steps, state } = queue.shift()!

        for (const next of nextStates(state)) {
            const serialized = serialize(next)
            if (seen.has(serialized)) {
                continue
            }
            seen.add(serialized)

            if (isFinalState(next)) {
                return steps + 1
            }

            queue.push({ steps: steps + 1, state: next })
        }
    }

    throw new Error('No solution found')
}

function addPart2ItemsToGroundFloor(state: State): State {
    const groundFloor = new Set<Item>([
        ...state.floors[0],
        { element: 'EL', type: 'chip' },
        { element: 'EL', type: 'generator' },
        { element: 'DI', type: 'chip' },
        { element: 'DI', type: 'generator' },
    ])

    return {
        elevator: state.elevator,
        floors: [groundFloor, ...state.floors.slice(1)],
    }
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
    const canGoUp = state.elevator < 3
    const canGoDown = state.elevator > 0

    if (canGoUp) {
        states.push(...findNextStates(state, state.elevator + 1))
    }

    if (canGoDown) {
        // only move items down if there are other items below (to get them)
        const floorsBelow = state.floors.slice(0, state.elevator)
        if (floorsBelow.some((f) => f.size > 0)) {
            states.push(...findNextStates(state, state.elevator - 1))
        }
    }

    return states
}

function findNextStates(state: State, target: number): State[] {
    const goingUp = state.elevator < target
    const currentFloorItems = [...state.floors[state.elevator]]
    const targetFloorItems = [...state.floors[target]]

    const safeCandidates = []
    let singleItemsToCheck = [...currentFloorItems]

    for (const pair of pairs(currentFloorItems)) {
        if (isSafe([...pair, ...targetFloorItems])) {
            if (goingUp) {
                // when going up, we take a safe pair of two items instead of
                // them individually to reduce steps
                safeCandidates.push(pair)
            } else {
                // when going down, we rather take only one of them down with us
                safeCandidates.push([pair[0]])
                safeCandidates.push([pair[1]])
            }
            // avoid checking safety of the two items individually
            singleItemsToCheck = singleItemsToCheck.filter(
                (item) => item !== pair[0] && item !== pair[1]
            )
        }
    }

    for (const item of singleItemsToCheck) {
        if (isSafe([item, ...targetFloorItems])) {
            safeCandidates.push([item])
        }
    }

    return safeCandidates.map((candidate) => {
        const itemsOfCurrentFloor = new Set(
            currentFloorItems.filter((item) => !candidate.includes(item))
        )

        const itemsOfTargetFloor = new Set([
            ...state.floors[target],
            ...candidate,
        ])

        return goingUp
            ? {
                  elevator: target,
                  floors: [
                      ...state.floors.slice(0, state.elevator),
                      itemsOfCurrentFloor,
                      itemsOfTargetFloor,
                      ...state.floors.slice(target + 1),
                  ],
              }
            : {
                  elevator: target,
                  floors: [
                      ...state.floors.slice(0, target),
                      itemsOfTargetFloor,
                      itemsOfCurrentFloor,
                      ...state.floors.slice(state.elevator + 1),
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

export function pairs<T>(items: T[]): [T, T][] {
    const result: [T, T][] = []

    for (let i = 0; i < items.length; i++) {
        for (let j = i + 1; j < items.length; j++) {
            result.push([items[i], items[j]])
        }
    }

    return result
}

export function serialize(state: State): string {
    // Here lies all the optimization: we only care about the number of single
    // items and chip/generator pairs on each floor.
    // This way, we treat states that are structurally the same as equal, which
    // significantly reduces the search space. This can be done, because to get
    // the number of steps to move everything to the top floor, we don't need to
    // know the exact position of each item.

    let result = String(state.elevator)

    for (const items of state.floors) {
        let singles = 0
        let pairs = 0

        for (const item of items) {
            const hasPairedItem = [...items].some(
                (other) =>
                    other !== item &&
                    other.element === item.element &&
                    other.type !== item.type
            )
            if (hasPairedItem) {
                pairs++
            } else {
                singles++
            }
        }

        result += `|${singles},${pairs / 2}`
    }

    return result
}

export function isFinalState(state: State): boolean {
    return (
        state.elevator === 3 &&
        state.floors.slice(0, 3).every((f) => f.size === 0)
    )
}
