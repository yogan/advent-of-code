import unittest, sys
from collections import defaultdict

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
sys.argv  = sys.argv[:1] # strip args, they scare the unittest module
is_sample = filename != "input.txt"

def parse(filename=filename):
    with open(filename) as f:
        elves = set()
        for x, line in enumerate(f):
            for y, c in enumerate(line):
                if c == "#":
                    elves.add((x, y))
        return elves

def dimensions(elves):
    min_x = min(x for x, y in elves)
    max_x = max(x for x, y in elves)
    min_y = min(y for x, y in elves)
    max_y = max(y for x, y in elves)

    return (min_x, max_x, min_y, max_y)

def print_field(elves):
    (min_x, max_x, min_y, max_y) = dimensions(elves)

    for x in range(min_x, max_x + 1):
        for y in range(min_y, max_y + 1):
            print("#" if (x, y) in elves else ".", end="")
        print()

deltas = (-1, 0, 1)

def neighbors(x, y):
    return {(x+dx, y+dy) for dx in deltas for dy in deltas if dx or dy}

def north_area(x, y):
    return {(x-1, y+dy) for dy in deltas}

def south_area(x, y):
    return {(x+1, y+dy) for dy in deltas}

def west_area(x, y):
    return {(x+dx, y-1) for dx in deltas}

def east_area(x, y):
    return {(x+dx, y+1) for dx in deltas}

def north(x, y):
    return (x-1, y)

def south(x, y):
    return (x+1, y)

def west(x, y):
    return (x, y-1)

def east(x, y):
    return (x, y+1)

class Dir:
    North = 0
    South = 1
    West  = 2
    East  = 3

def directions(rnd):
    assert rnd > 0, "rounds are counted starting from 1"
    match (rnd - 1) % 4:
        case 0: return [Dir.North, Dir.South, Dir.West, Dir.East]
        case 1: return [Dir.South, Dir.West, Dir.East, Dir.North]
        case 2: return [Dir.West, Dir.East, Dir.North, Dir.South]
        case 3: return [Dir.East, Dir.North, Dir.South, Dir.West]

def no_neighbors(elf, elves):
    x, y = elf
    free_neighbors = neighbors(x, y) - elves
    return len(free_neighbors) == 8

def area_free(area, elves):
    free_area = area - elves
    return len(free_area) == 3

def scan(elf, direction):
    x, y = elf
    match direction:
        case Dir.North: return (north_area(x, y), north(x, y))
        case Dir.South: return (south_area(x, y), south(x, y))
        case Dir.West:  return ( west_area(x, y),  west(x, y))
        case Dir.East:  return ( east_area(x, y),  east(x, y))
    assert False, f"invalid direction {direction}"

def move_proposals(elves, rnd):
    dirs = directions(rnd)
    proposals = dict()

    for elf in elves:
        if no_neighbors(elf, elves):
            proposals[elf] = elf
            continue

        for d in dirs:
            (area, proposal) = scan(elf, d)
            if area_free(area, elves):
                proposals[elf] = proposal
                break

        if elf not in proposals:
            proposals[elf] = elf

    return proposals

def count_empty_tiles(elves):
    (min_x, max_x, min_y, max_y) = dimensions(elves)
    return (max_x - min_x + 1) * (max_y - min_y + 1) - len(elves)

def part1():
    elves = parse()
    rounds = 10

    for rnd in range(1, rounds + 1):
        proposals = move_proposals(elves, rnd)

        counts = defaultdict(int)
        for target in proposals.values():
            counts[target] += 1

        elves = {current if counts[proposal] > 1 else proposal
                 for current, proposal in proposals.items()}

        # if is_sample:
        #     print(f"\n== End of Round {rnd} ==")
        #     print_field(elves)
        #     print()

    return count_empty_tiles(elves)

class TestDay23(unittest.TestCase):
    def test_parse(self):
        elves = parse()
        self.assertEqual(22, len(elves))
        for elf in [(0, 4), (1, 2), (1, 3), (1, 4), (1, 6), (6, 1), (6, 4)]:
            self.assertIn(elf, elves)
        for elf in [(0, 3), (0, 5), (1, 0), (1, 7), (6, 0), (6, 6), (9, 9)]:
            self.assertNotIn(elf, elves)

    def test_neighbors(self):
        expected = {(0, 0), (0, 1), (0, 2),
                    (1, 0),         (1, 2),
                    (2, 0), (2, 1), (2, 2)}
        self.assertEqual(expected, neighbors(1, 1))

    def test_north_area(self):
        expected = {(0, 0), (0, 1), (0, 2)}
        self.assertEqual(expected, north_area(1, 1))

    def test_south_area(self):
        expected = {(2, 0), (2, 1), (2, 2)}
        self.assertEqual(expected, south_area(1, 1))

    def test_west_area(self):
        expected = {(0, 0), (1, 0), (2, 0)}
        self.assertEqual(expected, west_area(1, 1))

    def test_east_area(self):
        expected = {(0, 2), (1, 2), (2, 2)}
        self.assertEqual(expected, east_area(1, 1))

    def test_directions(self):
        self.assertEqual([Dir.North, Dir.South, Dir.West, Dir.East], directions(1))
        self.assertEqual([Dir.South, Dir.West, Dir.East, Dir.North], directions(2))
        self.assertEqual([Dir.West, Dir.East, Dir.North, Dir.South], directions(3))
        self.assertEqual([Dir.East, Dir.North, Dir.South, Dir.West], directions(4))

        self.assertEqual([Dir.North, Dir.South, Dir.West, Dir.East], directions(5))
        self.assertEqual([Dir.South, Dir.West, Dir.East, Dir.North], directions(6))
        self.assertEqual([Dir.West, Dir.East, Dir.North, Dir.South], directions(7))
        self.assertEqual([Dir.East, Dir.North, Dir.South, Dir.West], directions(8))

        self.assertEqual([Dir.North, Dir.South, Dir.West, Dir.East], directions(9))

    def test_move_proposals_lonely_elf(self):
        elf = (5, 5)
        self.assertEqual({elf: elf}, move_proposals({elf}, 1))
        self.assertEqual({elf: elf}, move_proposals({elf}, 2))

    def test_move_proposals_small_example(self):
        # ............
        # ..##......#. <- lonely bonus elf :-(
        # ..#.........
        # ............
        # ..##........
        # ............
        elves = {(1, 2), (1, 3), (1, 10), (2, 2), (4, 2), (4, 3)}
        rnd = 1
        expected = {
            (1,  2): (0,  2), # north
            (1,  3): (0,  3), # north
            (1, 10): (1, 10), # stays
            (2,  2): (3,  2), # south (clash)
            (4,  2): (3,  2), # south (clash)
            (4,  3): (3,  3), # north
        }
        # self.assertEqual(expected, move_proposals(elves, rnd))

    def test_move_proposals_blocked(self):
        #   .....
        # 1 ...#.
        # 2 ..#.. <- blocked in all four directions
        # 3 .#...
        #   .....
        elves = {(1, 3), (2, 2), (3, 1)}
        rnd = 4 # prefer east
        expected = {
            (1, 3): (1, 4), # east
            (2, 2): (2, 2), # stays (blocked)
            (3, 1): (4, 1), # east blocked, north blocked, south free, so south
        }
        self.assertEqual(expected, move_proposals(elves, rnd))

    def test_part1(self):
        self.assertEqual(110 if is_sample else 3970, part1())

if __name__ == '__main__':
    if is_sample:
        unittest.main(exit=False)
        print("─" * 70)

    res1 = part1()
    print(f"Part 1: {res1}", "(sample)" if is_sample else "")
    # print()

    # res2 = part2()
    # print(f"Part 2: {res2}", "(sample)" if is_sample else "")
