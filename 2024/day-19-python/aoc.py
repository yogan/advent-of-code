import sys
import unittest


def parse(filename):
    patterns, designs = open(filename).read().strip().split("\n\n")
    patterns = patterns.split(", ")
    designs = designs.split("\n")
    return patterns, designs


def is_possible(patterns, design):
    if not design:
        return True

    rests = []

    for pattern in patterns:
        if design.startswith(pattern):
            rests.append(design[len(pattern) :])

    return any(is_possible(patterns, rest) for rest in rests)


class Tests(unittest.TestCase):
    def test_is_possible(self):
        patterns = ["r", "wr", "b", "g", "bwu", "rb", "gb", "br"]
        self.assertTrue(is_possible(patterns, "brwrr"))
        self.assertTrue(is_possible(patterns, "bggr"))
        self.assertTrue(is_possible(patterns, "gbbr"))
        self.assertTrue(is_possible(patterns, "rrbgbr"))
        self.assertFalse(is_possible(patterns, "ubwu"))
        self.assertTrue(is_possible(patterns, "bwurrg"))
        self.assertTrue(is_possible(patterns, "brgr"))
        self.assertFalse(is_possible(patterns, "bbrgwb"))


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

    patterns, designs = parse(filename)
    part1 = sum(is_possible(patterns, design) for design in designs)
    part2 = None

    check(1, part1, 6 if is_sample else 276)
    check(2, part2)
