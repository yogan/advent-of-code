import unittest, sys

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
sys.argv  = sys.argv[:1] # strip args, they scare the unittest module
is_sample = filename != "input.txt"

# NOTE: the numbers are NOT unique!
def parse(filename=filename):
    with open(filename) as f:
        return [int(line.strip()) for line in f.readlines()]

def to_indexed_pairs(numbers):
    return [(n, idx) for idx, n in enumerate(numbers)]

def from_indexed_pairs(pairs):
    return [n for n, _ in pairs]

def print_nums(pairs):
    print(", ".join([f"{n:5}" for n in from_indexed_pairs(pairs)][:25]))

def new_index(idx, num, length):
    if num == 0:
        return idx

    new_index = idx + (num % (length - 1))

    if new_index < 0:
        return new_index + length - 1
    elif new_index >= length:
        return new_index - length + 1

    if num < 0 and new_index == 0:
        return length - 1

    return new_index

def move(pairs, n_orig, idx_orig):
    if n_orig == 0:
        return pairs # noop for 0

    length = len(pairs)

    for pos, (n, idx) in enumerate(pairs):
        if (n, idx) == (n_orig, idx_orig):
            new_pos = new_index(pos, n, length)

            pair_removed = pairs[:pos] + pairs[pos+1:]

            return pair_removed[:new_pos] \
                    + [(n, idx)] \
                    + pair_removed[new_pos:]

def find_coordinates(numbers):
    pairs = to_indexed_pairs(numbers)

    for number, idx in pairs:
        pairs = move(pairs, number, idx)
        # print_nums(pairs)

    for i, (n, _) in enumerate(pairs):
        if n == 0:
            mod = len(pairs)
            return [pairs[(i + x) % mod][0] for x in [1000, 2000, 3000]]

    assert(False)

def part1():
    numbers = parse()
    coordinates = find_coordinates(numbers)
    print("Coordinates:", coordinates)
    return sum(coordinates)

class TestDay20(unittest.TestCase):
    sample = [1, 2, -3, 3, -2, 0, 4]

    def test_parse(self):
        numbers = parse()
        if is_sample:
            self.assertEqual(self.sample, numbers)
        else:
            self.assertEqual(5000, len(numbers))

    def test_move_sample(self):
        pairs = to_indexed_pairs(self.sample)
        results = []

        for number, idx in pairs:
            pairs = move(pairs, number, idx)
            results.append(from_indexed_pairs(pairs))

        self.assertEqual([
            [2, 1, -3, 3, -2, 0, 4],
            [1, -3, 2, 3, -2, 0, 4],
            [1, 2, 3, -2, -3, 0, 4],
            [1, 2, -2, -3, 0, 3, 4],
            [1, 2, -3, 0, 3, 4, -2],
            [1, 2, -3, 0, 3, 4, -2],
            [1, 2, -3, 4, 0, 3, -2],
        ], results)

    def test_find_coordinates(self):
        numbers = parse()
        coordinates = find_coordinates(numbers)
        if is_sample:
            self.assertEqual([4, -3, 2], coordinates)
        else:
            self.assertEqual([9989, -2204, -5582], coordinates)


if __name__ == '__main__':
    unittest.main(exit=False)
    print()

    res1 = part1()
    print(f"Part 1: {res1}", "(sample)" if is_sample else "")

    # res2 = part2()
    # print(f"Part 2: {res2}", "(sample)" if is_sample else "")
