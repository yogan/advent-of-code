import sys, unittest

if len(sys.argv) != 2:
    print("Missing input file.")
    exit(1)
filename  = sys.argv[1]
sys.argv  = sys.argv[:1] # strip args, they confuse the unittest module
is_sample = filename == "sample.txt"

def parse():
    lines = [x.strip() for x in open(filename).readlines()]
    return list(map(parse_line, lines))

def parse_line(line):
    return [int(x) for x in line.split("x")]

def volume(dimensions):
    l, w, h = dimensions
    return l * w * h

class TestDayXX(unittest.TestCase):
    def test_parse_line(self):
        self.assertEqual(parse_line("1x23x420"), [1, 23, 420])

def check(part, actual, expected=None):
    print(f"Part {part}{' (sample)' if is_sample else ''}: {actual} ", end="")
    if expected is None:
        print("❔")
    else:
        if actual != expected:
            print(f"≠ {expected} ❌")
            exit(1)
        print("✅")

if __name__ == '__main__':
    unittest.main(exit=False)
    print()

    lines = parse()
    part1 = sum([volume(x) for x in lines])
    part2 = None

    check(1, part1, 9876 if is_sample else 14545)
    check(2, part2)
