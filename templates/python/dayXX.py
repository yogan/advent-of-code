import sys, unittest

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
sys.argv  = sys.argv[:1] # strip args, they scare the unittest module
is_sample = filename == "sample.txt"

def parse():
    with open(filename) as f:
        lines = [x.strip() for x in f.readlines()]
    return list(map(parse_line, lines))

def parse_line(line):
    return [int(x) for x in line.split("x")]

def surface_area(dimensions):
    l, w, h = dimensions
    return 2 * (l * w + w * h + h * l)

def volume(dimensions):
    l, w, h = dimensions
    return l * w * h

def part1(lines):
    return sum([volume(x) for x in lines])

def part2(lines):
    return sum([surface_area(x) for x in lines])

class TestDayXX(unittest.TestCase):
    def test_parse_line(self):
        self.assertEqual(parse_line("1x23x420"), [1, 23, 420])

    def test_part1(self):
        self.assertEqual(part1([[1, 2, 3], [1, 1, 1]]), 6 + 1)

    def test_part2(self):
        expected = (2 + 2 + 3 + 3 + 6 + 6) + (6 * 1)
        self.assertEqual(part2([[1, 2, 3], [1, 1, 1]]), expected)

def print_and_assert(part, expected, actual):
    print(f"Part {part}: {actual}{' (sample)' if is_sample else ''}")
    assert actual == expected, f"{part} was {actual}, expected {expected}"

if __name__ == '__main__':
    unittest.main(exit=False)
    print()

    lines = parse()

    print_and_assert(1, 9876 if is_sample else 14545, part1(lines))
    print_and_assert(2, 21756 if is_sample else 4978, part2(lines))
