import sys
import unittest


def parse():
    return [parse_line(line) for line in open(filename).readlines()]


def parse_line(line):
    target, rest = line.split(": ")
    return int(target), list(map(int, rest.split(" ")))


def possible(target, rest):
    blanks = len(rest) - 1

    # Go through list of all possible combinations of operators (+ and *).
    # The input has a maximum of 11 numbers, so there are at most 2^11 = 2048
    # possibilities, which is fine to brute force.
    # We use an increasing integer number as bitmask, where a 0 bit means + and
    # a 1 bit means *.
    for bitmask in range(2**blanks):
        result = rest[0]

        for i in range(blanks):
            # Extract the i-th bit from the bitmask; multiply if 1, add if 0.
            if (bitmask >> i) & 1:
                result *= rest[i + 1]
            else:
                result += rest[i + 1]

        if result == target:
            return True

    return False


def part1(lines):
    return sum(target for target, rest in lines if possible(int(target), rest))


class Tests(unittest.TestCase):
    def test_parse_line(self):
        self.assertEqual(parse_line("21037: 9 7 18 13"), (21037, [9, 7, 18, 13]))

    def test_possible(self):
        self.assertTrue(possible(190, [10, 19]))
        self.assertTrue(possible(3267, [81, 40, 27]))
        self.assertFalse(possible(83, [17, 5]))
        self.assertFalse(possible(156, [15, 6]))
        self.assertFalse(possible(7290, [6, 8, 6, 15]))
        self.assertFalse(possible(161011, [16, 10, 13]))
        self.assertFalse(possible(192, [17, 8, 14]))
        self.assertFalse(possible(21037, [9, 7, 18, 13]))
        self.assertTrue(possible(292, [11, 6, 16, 20]))


if __name__ == "__main__":
    flags = set(arg for arg in sys.argv[1:] if arg.startswith("-"))
    args = [arg for arg in sys.argv[1:] if not arg.startswith("-")]
    sys.argv = sys.argv[:1]  # strip args, unittest.main() doesn't like them

    filename = "sample.txt" if "-s" in flags or "--sample" in flags else "input.txt"
    filename = args[0] if args else filename
    is_sample = filename.startswith("sample")
    run_tests = "-t" in flags or "--test" in flags

    if run_tests:
        unittest.main(exit=True)

    def check(part, actual, expected=None):
        print(f"Part {part}{' (sample)' if is_sample else ''}: {actual} ", end="")
        if expected is None:
            print("❔")
        else:
            if actual != expected:
                print(f"≠ {expected} ❌")
                exit(1)
            print("✅")

    lines = parse()
    p1 = part1(lines)
    p2 = None

    check(1, p1, 3749 if is_sample else 663613490587)
    check(2, p2)
