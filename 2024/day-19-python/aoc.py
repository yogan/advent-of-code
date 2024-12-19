import sys
import unittest


def parse(filename):
    patterns, designs = open(filename).read().strip().split("\n\n")
    patterns = patterns.split(", ")
    designs = designs.split("\n")
    return patterns, designs


def combinations(patterns, design, cache={}):
    if design in cache:
        return cache[design]

    if not design:
        cache[design] = 1
        return 1

    rests = []

    for pattern in patterns:
        if design.startswith(pattern):
            rests.append(design[len(pattern) :])

    count = sum(combinations(patterns, rest) for rest in rests)
    cache[design] = count
    return count


class Tests(unittest.TestCase):
    def test_combinations(self):
        patterns = ["r", "wr", "b", "g", "bwu", "rb", "gb", "br"]
        self.assertEqual(combinations(patterns, "brwrr"), 2)
        self.assertEqual(combinations(patterns, "bggr"), 1)
        self.assertEqual(combinations(patterns, "gbbr"), 4)
        self.assertEqual(combinations(patterns, "rrbgbr"), 6)
        self.assertEqual(combinations(patterns, "ubwu"), 0)
        self.assertEqual(combinations(patterns, "bwurrg"), 1)
        self.assertEqual(combinations(patterns, "brgr"), 2)
        self.assertEqual(combinations(patterns, "bbrgwb"), 0)


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
    part1 = sum(1 for design in designs if combinations(patterns, design))
    part2 = sum(combinations(patterns, design) for design in designs)

    check(1, part1, +6 if is_sample else 276)
    check(2, part2, 16 if is_sample else 681226908011510)
