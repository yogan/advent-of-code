import sys
import unittest


def parse(filename):
    return list(open(filename).read().strip())


def part1(letters):
    return count_pairs(letters, "a")


def part2(letters):
    return (
        count_pairs(letters, "a")
        + count_pairs(letters, "b")
        + count_pairs(letters, "c")
    )


def count_pairs(letters, char):
    letters = [c for c in letters if c.lower() == char]
    pairs = 0

    for i, c in enumerate(letters):
        if c == char:
            pairs += len([c for c in letters[:i] if c == char.upper()])

    return pairs


class Tests(unittest.TestCase):
    def test_part1(self):
        self.assertEqual(part1(list("ABabACacBCbca")), 5)

    def test_part2(self):
        self.assertEqual(part2(list("ABabACacBCbca")), 11)


def main():
    failures = 0

    if is_sample:
        failures += check(1, part1(parse("sample1.txt")), 5)
        failures += check(2, part2(parse("sample2.txt")), 11)
    else:
        failures += check(1, part1(parse("input1.txt")), 151)
        failures += check(2, part2(parse("input2.txt")), 3747)

    exit(failures)


def check(part, actual, expected=None):
    failure = 0

    if expected is None:
        symbol = "🤔"
        result = f"{actual}"
    elif actual == expected:
        symbol = "✅"
        result = f"{actual}"
    else:
        symbol = "❌"
        result = f"{actual} ≠ {expected}"
        failure = 1

    print(f"{symbol} Part {part}{' (sample)' if is_sample else ''}: {result}")
    return failure


if __name__ == "__main__":
    flags = set(arg for arg in sys.argv[1:] if arg.startswith("-"))

    is_sample = "-s" in flags or "--sample" in flags
    run_tests = "-t" in flags or "--test" in flags

    if run_tests:
        sys.argv = sys.argv[:1]  # strip args, unittest.main() doesn't like them
        unittest.main(exit=True)

    main()
