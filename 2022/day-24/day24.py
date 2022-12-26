import unittest, sys
from math import gcd
from collections import deque
from tqdm import tqdm

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
sys.argv  = sys.argv[:1] # strip args, they scare the unittest module
is_input  = filename == "input.txt"
is_sample = not is_input

def parse(filename=filename):
    with open(filename) as f:
        valley = []
        for line in f:
            row = []
            for c, entry in enumerate(line.strip()):
                match(entry):
                    case '#':
                        row.append(entry)
                    case '.':
                        row.append([])
                    case _:
                        row.append(list(entry))
            valley.append(row)
        return valley

def print_valley(valley, pos):
    for r, row in enumerate(valley):
        line = " "
        for c, entry in enumerate(row):
            if (r, c) == pos:
                line += "E"
            else:
                match(entry):
                    case '#':
                        line += entry
                    case []:
                        line += "."
                    case [x]:
                        line += str(x)
                    case [*_]:
                        line += str(len(entry))
                    case _:
                        raise Exception(f"Unknown entry: {entry}")
        print(line)
    print()

def find_start_and_end(valley):
    start, end = None, None
    for c, entry in enumerate(valley[0]):
        if entry == []:
            start = (0, c)
    for c, entry in enumerate(valley[-1]):
        if entry == []:
            end = (len(valley) - 1, c)
    return start, end

def empty_valley(valley):
    next_valley = []

    for r, row in enumerate(valley):
        # top/bottom walls
        if r == 0 or r == len(valley) - 1:
            next_valley.append(row) # no need to copy, they don't change
            continue

        # empty ([]) valley rows, keeping the walls (#) on the sides
        next_row = []
        for c, entry in enumerate(row):
            next_row.append(entry if entry == '#' else [])
        next_valley.append(next_row)

    return next_valley

def next_coords(r, c, blizzard, valley):
    match blizzard:
        case 'v':
            # height 7, 0-6, valley is 1-5
            r2 = r + 1
            if r2 == len(valley) - 1:
                r2 = 1
            return r2, c
        case '^':
            r2 = r - 1
            if r2 == 0:
                r2 = len(valley) - 2
            return r2, c
        case '>':
            # width 10, 0-9, valley is 1-8
            c2 = c + 1
            if c2 == len(valley[0]) - 1:
                c2 = 1
            return r, c2
        case '<':
            c2 = c - 1
            if c2 == 0:
                c2 = len(valley[0]) - 2
            return r, c2
        case _:
            raise Exception(f"Unknown blizzard: {blizzard}")

def get_next(valley):
    next_valley = empty_valley(valley)

    for r, row in enumerate(valley):
        if r == 0 or r == len(valley) - 1:
            continue # skip top/bottom walls

        for c, entry in enumerate(row):
            if c == 0 or c == len(row) - 1:
                continue # skip side walls

            for blizzard in entry:
                r2, c2 = next_coords(r, c, blizzard, valley)
                next_valley[r2][c2].append(blizzard)

    return next_valley

def free_positions(valley, pos, end):
    r, c = pos
    candidates = [
        (r - 1, c),
        (r, c - 1),
        (r, c),     # staying is also an option
        (r, c + 1),
        (r + 1, c),
    ]
    free = []
    for r, c in candidates:
        if (r, c) == end:
            return [end]
        if r < 1 or c < 1 or r > len(valley) - 1 or c > len(valley[r]) - 1:
            continue
        if valley[r][c] == []:
            free.append((r, c))
    return free

def calc_cycle(valley):
    # Worst case: horizontal and vertical blizzards are not in sync at all,
    # so the get back to their starting position after width * height steps.
    # But we can easily check if that happens earlier by dividing by the GCD
    # of width and height.
    walls  = 2
    height = len(valley)    - walls
    width  = len(valley[0]) - walls
    return width * height // gcd(width, height)

def bfs(valley, pos, end):
    cycles = calc_cycle(valley)

    visited = set()
    visited.add((0, pos))

    queue = deque()
    queue.append((0, pos, valley))

    if not is_sample:
        estimate = 373  # once calculated, the "estimate" is quite precise ;-)
        bar_format = "{desc}: {n_fmt}/{total_fmt} {percentage:3.0f}%" \
                   + "|{bar}|[{elapsed}<{remaining}]"
        t = tqdm(total=estimate, desc="Minute", bar_format=bar_format,
                 smoothing=0.1)

    while queue:
        minute, pos, valley = queue.popleft()

        if not is_sample:
            t.update(minute - t.n)

        valley = get_next(valley)
        free = free_positions(valley, pos, end)

        if end in free:
            print("Visited states:", len(visited))
            return minute + 1

        for next_pos in free:
            q_entry = (minute + 1, next_pos, valley)
            v_entry = ((minute + 1) % cycles, next_pos)
            if v_entry not in visited:
                queue.append(q_entry)
                visited.add(v_entry)

    assert False, "got lost in the blizzards"

def part1():
    valley = parse()
    (pos), (end) = find_start_and_end(valley)

    return bfs(valley, pos, end)

class TestDay24(unittest.TestCase):
    def test_parse_matches_dimensions(self):
        valley = parse()
        self.assertEqual(22, len(valley))
        for row in valley:
            self.assertEqual(152, len(row))

    def test_parse_internal_representation_is_mixed_lists_and_strings(self):
        valley = parse()

        # first 6 chars of first row (start): "#.####"
        self.assertEqual(['#', [], '#', '#', '#', '#'], valley[0][:6])

        # last 6 chars of last row (end): "####.#"
        self.assertEqual(['#', '#', '#', '#', [], '#'], valley[-1][-6:])

        # start of first row in the valley: "#>>>v.<>"
        self.assertEqual(
            ['#', ['>'], ['>'], ['>'], ['v'], [], ['<'], ['>']],
            valley[1][:8])

        # end of first row in the valley: "^^<>^.#"
        self.assertEqual(
            [['<'], ['^'], ['^'], ['<'], ['>'], ['^'], [], '#'],
            valley[1][-8:])

    def test_find_start_and_end(self):
        valley = parse()
        (start), (end) = find_start_and_end(parse())
        self.assertEqual((0, 1), start)
        self.assertEqual((21, 150), end)

    def test_calc_cycle(self):
        valley = parse()
        # 150 × 20 = 3000, but GCD = 10, so 300
        # I've actually double-checked that this is correct by running the
        # simulation for many steps and checking when we reach blizzard
        # that have been seen before. bil
        self.assertEqual(300, calc_cycle(valley))

if __name__ == '__main__':
    if is_input:
        unittest.main(exit=False)
        print("─" * 70)

    res1 = part1()
    if is_sample:
        assert res1 == 18, res1
    print(f"Part 1: {res1}", "(sample)" if is_sample else "")

    # print()
    # res2 = part2()
    # print(f"Part 2: {res2}", "(sample)" if is_sample else "")
