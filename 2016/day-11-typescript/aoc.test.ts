import { describe, expect, it } from 'bun:test'
import {
    Item,
    State,
    combinations,
    serialize,
    isSafe,
    nextStates,
    parseLine,
    isFinalState,
    part1,
} from './aoc'

describe('parseLine', () => {
    const sample = [
        'The first floor contains a hydrogen-compatible microchip' +
            ' and a lithium-compatible microchip.',
        'The second floor contains a hydrogen generator.',
        'The third floor contains a lithium generator.',
        'The fourth floor contains nothing relevant.',
    ] as const

    it('works for the first floor of the sample input', () => {
        expect(parseLine(sample[0])).toEqual(
            new Set<Item>([
                { type: 'chip', element: 'HY' },
                { type: 'chip', element: 'LI' },
            ])
        )
    })

    it('works for the second floor of the sample input', () => {
        expect(parseLine(sample[1])).toEqual(
            new Set<Item>([{ type: 'generator', element: 'HY' }])
        )
    })

    it('works for the third floor of the sample input', () => {
        expect(parseLine(sample[2])).toEqual(
            new Set<Item>([{ type: 'generator', element: 'LI' }])
        )
    })

    it('works for the fourth floor of the sample input', () => {
        expect(parseLine(sample[3])).toEqual(new Set())
    })

    it('works for comma separated items as found in the real input', () => {
        expect(
            parseLine(
                'The first floor contains a promethium generator,' +
                    ' a promethium-compatible microchip,' +
                    ' a ruthenium generator,' +
                    ' and a ruthenium-compatible microchip.'
            )
        ).toEqual(
            new Set<Item>([
                { type: 'chip', element: 'PR' },
                { type: 'chip', element: 'RU' },
                { type: 'generator', element: 'PR' },
                { type: 'generator', element: 'RU' },
            ])
        )
    })
})

describe('isSafe', () => {
    it('is true for no items', () => {
        expect(isSafe([])).toBe(true)
    })

    it('is true for single items', () => {
        expect(isSafe([{ type: 'chip', element: 'HY' }])).toBe(true)
        expect(isSafe([{ type: 'generator', element: 'HY' }])).toBe(true)
    })

    it('is true for a chip and its matching generator', () => {
        expect(
            isSafe([
                { type: 'chip', element: 'HY' },
                { type: 'generator', element: 'HY' },
            ])
        ).toBe(true)

        expect(
            isSafe([
                { type: 'chip', element: 'HY' },
                { type: 'generator', element: 'HY' },
                { type: 'generator', element: 'LI' },
            ])
        ).toBe(true)

        expect(
            isSafe([
                { type: 'chip', element: 'HY' },
                { type: 'chip', element: 'LI' },
                { type: 'generator', element: 'HY' },
                { type: 'generator', element: 'LI' },
            ])
        ).toBe(true)
    })

    it('is false for a chip and a non-matching generator', () => {
        expect(
            isSafe([
                { type: 'chip', element: 'HY' },
                { type: 'generator', element: 'LI' },
            ])
        ).toBe(false)

        expect(
            isSafe([
                { type: 'chip', element: 'HY' },
                { type: 'chip', element: 'LI' },
                { type: 'generator', element: 'HY' },
                { type: 'generator', element: 'PR' },
            ])
        ).toBe(false)
    })
})

describe('combinations', () => {
    it('works on an empty list', () => {
        expect(combinations([], 0)).toEqual([[]])
        expect(combinations([], 1)).toEqual([])
        expect(combinations([], 2)).toEqual([])
        expect(combinations([], 99)).toEqual([])
    })

    it('works for singles', () => {
        expect(combinations([1, 2, 3], 1)).toEqual([[1], [2], [3]])
    })

    it('works for pairs', () => {
        expect(combinations([1, 2, 3], 2)).toEqual([
            [1, 2],
            [1, 3],
            [2, 3],
        ])
    })

    it('works for triples', () => {
        expect(combinations([1, 2], 3)).toEqual([])
        expect(combinations([1, 2, 3], 3)).toEqual([[1, 2, 3]])
        expect(combinations([1, 2, 3, 4], 3)).toEqual([
            [1, 2, 3],
            [1, 2, 4],
            [1, 3, 4],
            [2, 3, 4],
        ])
    })
})

const initialSampleState: State = {
    elevator: 0,
    floors: [
        new Set<Item>([
            { type: 'chip', element: 'HY' },
            { type: 'chip', element: 'LI' },
        ]),
        new Set<Item>([{ type: 'generator', element: 'HY' }]),
        new Set<Item>([{ type: 'generator', element: 'LI' }]),
        new Set<Item>(),
    ],
}

describe('nextStates', () => {
    it('works for the initial state of the sample input', () => {
        // - we can only move from 0 to 1
        // - elevator needs at least one item for power, but also has a cap of 2
        // - so, we could move either HY or LI, or both
        // - but we can't bring the LI chip, as there is no LI generator on the
        //   1st floor, so it would be destroyed
        // -> only possible move is to bring HY chip from 0 to 1
        const expectedStates: State[] = [
            {
                elevator: 1,
                floors: [
                    new Set<Item>([{ type: 'chip', element: 'LI' }]),
                    new Set<Item>([
                        { type: 'generator', element: 'HY' },
                        { type: 'chip', element: 'HY' },
                    ]),
                    new Set<Item>([{ type: 'generator', element: 'LI' }]),
                    new Set<Item>(),
                ],
            },
        ]

        expect(nextStates(initialSampleState)).toEqual(expectedStates)
    })

    it('works for the second state of the sample input', () => {
        const state: State = {
            elevator: 1,
            floors: [
                new Set<Item>([{ type: 'chip', element: 'LI' }]),
                new Set<Item>([
                    { type: 'generator', element: 'HY' },
                    { type: 'chip', element: 'HY' },
                ]),
                new Set<Item>([{ type: 'generator', element: 'LI' }]),
                new Set<Item>(),
            ],
        }
        const next = nextStates(state)

        expect(next.length).toBe(3)
        expect(next).toContainEqual({
            elevator: 2,
            floors: [
                new Set<Item>([{ type: 'chip', element: 'LI' }]),
                new Set<Item>([{ type: 'chip', element: 'HY' }]),
                new Set<Item>([
                    { type: 'generator', element: 'LI' },
                    { type: 'generator', element: 'HY' },
                ]),
                new Set<Item>(),
            ],
        })
        expect(next).toContainEqual({
            elevator: 2,
            floors: [
                new Set<Item>([{ type: 'chip', element: 'LI' }]),
                new Set<Item>([]),
                new Set<Item>([
                    { type: 'generator', element: 'LI' },
                    { type: 'generator', element: 'HY' },
                    { type: 'chip', element: 'HY' },
                ]),
                new Set<Item>(),
            ],
        })
        expect(next).toContainEqual({
            elevator: 0,
            floors: [
                new Set<Item>([
                    { type: 'chip', element: 'LI' },
                    { type: 'chip', element: 'HY' },
                ]),
                new Set<Item>([{ type: 'generator', element: 'HY' }]),
                new Set<Item>([{ type: 'generator', element: 'LI' }]),
                new Set<Item>(),
            ],
        })
    })
})

describe('serialize', () => {
    it('works for a state with no items', () => {
        expect(
            serialize({
                elevator: 23,
                floors: [
                    new Set<Item>(),
                    new Set<Item>(),
                    new Set<Item>(),
                    new Set<Item>(),
                ],
            })
        ).toBe('23||||')
    })

    it('works for a state with one item per floor', () => {
        expect(
            serialize({
                elevator: 1,
                floors: [
                    new Set<Item>([{ type: 'chip', element: 'LI' }]),
                    new Set<Item>([{ type: 'generator', element: 'HY' }]),
                    new Set<Item>([{ type: 'generator', element: 'LI' }]),
                    new Set<Item>([{ type: 'chip', element: 'HY' }]),
                ],
            })
        ).toBe('1|LI-C|HY-G|LI-G|HY-C')
    })

    it('sorts items per floor alphabetically', () => {
        expect(
            serialize({
                elevator: 1,
                floors: [
                    new Set<Item>(),
                    new Set<Item>([
                        { type: 'generator', element: 'AB' },
                        { type: 'generator', element: 'XY' },
                        { type: 'generator', element: 'CD' },
                    ]),
                    new Set<Item>([
                        { type: 'chip', element: 'LI' },
                        { type: 'generator', element: 'LI' },
                        { type: 'generator', element: 'AB' },
                        { type: 'chip', element: 'AB' },
                        { type: 'chip', element: 'EF' },
                        { type: 'chip', element: 'CD' },
                    ]),
                    new Set<Item>([{ type: 'chip', element: 'HY' }]),
                ],
            })
        ).toBe('1||AB-G,CD-G,XY-G|AB-C,AB-G,CD-C,EF-C,LI-C,LI-G|HY-C')
    })
})

describe('isFinalState', () => {
    it('returns false for the initial state of the sample input', () => {
        expect(isFinalState(initialSampleState)).toBe(false)
    })

    it('returns true when all items are on the top floor', () => {
        const state: State = {
            elevator: 3,
            floors: [
                new Set<Item>(),
                new Set<Item>(),
                new Set<Item>(),
                new Set<Item>([
                    { type: 'chip', element: 'HY' },
                    { type: 'chip', element: 'LI' },
                    { type: 'generator', element: 'HY' },
                    { type: 'generator', element: 'LI' },
                ]),
            ],
        }

        expect(isFinalState(state)).toBe(true)
    })
})

describe('part1', () => {
    it('returns a minimum steps of 11 for the sample input', () => {
        expect(part1(initialSampleState)).toBe(11)
    })
})
