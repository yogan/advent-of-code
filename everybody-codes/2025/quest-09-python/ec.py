import sys
import unittest


def parse(filename):
    return [list(l[2:].strip()) for l in open(filename).readlines()]


def part1(sequences):
    x = matches(sequences[0], sequences[2])
    y = matches(sequences[1], sequences[2])
    return x * y


def matches(seq1, seq2):
    return sum(1 if x == y else 0 for x, y in zip(seq1, seq2))


class Tests(unittest.TestCase):
    def test_part1(self):
        seq1 = list("CAAGCGCTAAGTTCGCTGGATGTGTGCCCGCG")
        seq2 = list("CTTGAATTGGGCCGTTTACCTGGTTTAACCAT")
        seq3 = list("CTAGCGCTGAGCTGGCTGCCTGGTTGACCGCG")
        self.assertEqual(part1([seq1, seq2, seq3]), 414)

    def test_matches(self):
        seq1 = list("CAAGCGCTAAGTTCGCTGGATGTGTGCCCGCG")
        seq2 = list("CTTGAATTGGGCCGTTTACCTGGTTTAACCAT")
        seq3 = list("CTAGCGCTGAGCTGGCTGCCTGGTTGACCGCG")
        self.assertEqual(matches(seq1, seq3), 23)
        self.assertEqual(matches(seq2, seq3), 18)


def main():
    failures = 0

    if is_sample:
        failures += check(1, part1(parse("sample1.txt")), 414)
    else:
        failures += check(1, part1(parse("input1.txt")), 6364)

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
        unittest.main(argv=sys.argv[:1])

    main()
