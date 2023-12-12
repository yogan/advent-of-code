import sys, unittest, re

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
sys.argv  = sys.argv[:1] # strip args, they scare the unittest module
is_sample = filename == "sample.txt"

def parse():
    with open(filename) as f:
        lines = [x.strip().split() for x in f.readlines()]
    return [[re.sub(r"^\.+|\.+$", "", l[0]), # probably overkill optimization
             list(map(int, l[1].split(",")))]
            for l in lines]

def initial_offsets(lengths):
    assert len(lengths) > 0
    offsets = []
    cur = 0
    for l in lengths:
        offsets.append(cur)
        cur += l + 1
    return offsets

def get_shifts(max, length):
    if length == 1:
        return [[x] for x in range(max + 1)]
    else:
        return [y + [x] for x in range(max + 1) for y in get_shifts(x, length - 1)]

# NOTE: ranges are start inclusive, end exclusive: [start, end)
def possible_ranges(lengths, total_length):
    start_offsets = initial_offsets(lengths)
    padding = total_length - sum(lengths) - len(lengths) + 1
    shifts = get_shifts(padding, len(start_offsets))

    ranges = []

    for shift in shifts:
        r = []
        for i in range(len(start_offsets)):
            left = start_offsets[i] + shift[i]
            right = left + lengths[i]
            assert right <= total_length
            r.append((left, right))
        touches_or_overlaps = any([x[0][1] > x[1][0] for x in zip(r, r[1:])])
        assert not touches_or_overlaps, f"ranges touch or overlap: {r}"
        ranges.append(r)

    return ranges

def to_spring_string(ranges, total_length):
    springs = ""
    for i in range(total_length):
        springs += "#" if any([r[0] <= i < r[1] for r in ranges]) else "."
    return springs

def is_valid(pattern, candidate):
    assert len(pattern) == len(candidate)

    for i in range(len(pattern)):
        if pattern[i] == "#" and candidate[i] == ".":
            return False
        if pattern[i] == "." and candidate[i] == "#":
            return False

    return True

def part1(lines):
    arrangements_per_line = []

    for springs, lengths in lines:
        total_length = len(springs)
        ranges = possible_ranges(lengths, total_length)
        candidates = [to_spring_string(r, total_length) for r in ranges]
        arrangements = [c for c in candidates if is_valid(springs, c)]
        arrangements_per_line.append(len(arrangements))

    return sum(arrangements_per_line)

class TestDay(unittest.TestCase):
    def assert_arrays_equal(self, a, b):
        a.sort()
        b.sort()
        self.assertEqual(a, b)

    def test_shifts(self):
        self.assert_arrays_equal(get_shifts(1, 2), [[0, 0], [0, 1], [1, 1]])

        self.assert_arrays_equal(get_shifts(0, 3), [[0, 0, 0]])
        self.assert_arrays_equal(get_shifts(1, 3),
                                 [[0, 0, 0], [0, 0, 1], [0, 1, 1], [1, 1, 1]])
        self.assert_arrays_equal(get_shifts(2, 3), [
            [0, 0, 0], [0, 0, 1], [0, 0, 2], [0, 1, 1], [0, 1, 2], [0, 2, 2],
            [1, 1, 1], [1, 1, 2], [1, 2, 2], [2, 2, 2]])

        self.assert_arrays_equal(get_shifts(1, 4), [
            [0, 0, 0, 0], [0, 0, 0, 1], [0, 0, 1, 1],
            [0, 1, 1, 1], [1, 1, 1, 1]])

    def test_possible_ranges_many(self):
        self.assert_arrays_equal(possible_ranges([3, 2, 1], len("###.##.#...")), [
                #                             01234567890   SHIFT
                [(0, 3), (4, 6), ( 7,  8)], # ###.##.#...   0, 0, 0
                [(0, 3), (4, 6), ( 8,  9)], # ###.##..#..   0, 0, 1
                [(0, 3), (4, 6), ( 9, 10)], # ###.##...#.   0, 0, 2
                [(0, 3), (4, 6), (10, 11)], # ###.##....#   0, 0, 3
                [(0, 3), (5, 7), ( 8,  9)], # ###..##.#..   0, 1, 1
                [(0, 3), (5, 7), ( 9, 10)], # ###..##..#.   0, 1, 2
                [(0, 3), (5, 7), (10, 11)], # ###..##...#   0, 1, 3
                [(0, 3), (6, 8), ( 9, 10)], # ###...##.#.   0, 2, 2
                [(0, 3), (6, 8), (10, 11)], # ###...##..#   0, 2, 3
                [(0, 3), (7, 9), (10, 11)], # ###....##.#   0, 3, 3
                #                             01234567890
                [(1, 4), (5, 7), ( 8,  9)], # .###.##.#..   1, 1, 1
                [(1, 4), (5, 7), ( 9, 10)], # .###.##..#.   1, 1, 2
                [(1, 4), (5, 7), (10, 11)], # .###.##...#   1, 1, 3
                [(1, 4), (6, 8), ( 9, 10)], # .###..##.#.   1, 2, 2
                [(1, 4), (6, 8), (10, 11)], # .###..##..#   1, 2, 3
                [(1, 4), (7, 9), (10, 11)], # .###...##.#   1, 3, 3
                #                             01234567890
                [(2, 5), (6, 8), ( 9, 10)], # ..###.##.#.   2, 2, 2
                [(2, 5), (6, 8), (10, 11)], # ..###.##..#   2, 2, 3
                [(2, 5), (7, 9), (10, 11)], # ..###..##.#   2, 3, 3
                #                             01234567890
                [(3, 6), (7, 9), (10, 11)], # ...###.##.#   3, 3, 3
            ])

    def test_to_spring_string(self):
        length = 11
        self.assertEqual(to_spring_string(
            [(0, 3), (4, 6), ( 7,  8)], length), "###.##.#...")
        self.assertEqual(to_spring_string(
            [(1, 4), (6, 8), (10, 11)], length), ".###..##..#")
        self.assertEqual(to_spring_string(
            [(3, 6), (7, 9), (10, 11)], length), "...###.##.#")

def print_and_assert(part, expected, actual):
    print(f"Part {part}: {actual}{' (sample)' if is_sample else ''}")
    assert actual == expected, f"{part} was {actual}, expected {expected}"

if __name__ == '__main__':
    unittest.main(exit=False)
    print()
    # exit()

    lines = parse()

    print_and_assert(1, 21 if is_sample else 7169, part1(lines))
    # print_and_assert(2, 21756 if is_sample else 4978, part2(lines))
