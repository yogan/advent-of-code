import sys
import unittest


def parse(filename):
    return [list(l.strip().split(":")[1]) for l in open(filename).readlines()]


def part1(sequences):
    return score(*sequences)


def part2(sequences):
    total = 0

    for i in range(len(sequences)):
        for j in range(i + 1, len(sequences)):
            for ci, child in enumerate(sequences):
                if ci in [i, j]:
                    continue
                total += score(sequences[i], sequences[j], child)

    return total


def score(a, b, c):
    m1 = matches(a, c)
    m2 = matches(b, c)

    return sum(m1) * sum(m2) if all(merge(m1, m2)) else 0


def matches(seq1, seq2):
    return [1 if x == y else 0 for x, y in zip(seq1, seq2)]


def merge(seq1, seq2):
    return [1 if x + y >= 1 else 0 for x, y in zip(seq1, seq2)]


class Tests(unittest.TestCase):
    def s2m(self, str):
        return [1 if c == "+" else 0 for c in str]

    def test_part1(self):
        seq1 = list("CAAGCGCTAAGTTCGCTGGATGTGTGCCCGCG")
        seq2 = list("CTTGAATTGGGCCGTTTACCTGGTTTAACCAT")
        seq3 = list("CTAGCGCTGAGCTGGCTGCCTGGTTGACCGCG")
        self.assertEqual(part1([seq1, seq2, seq3]), 414)

    def test_part2(self):
        sequences = [
            list("GCAGGCGAGTATGATACCCGGCTAGCCACCCC"),
            list("TCTCGCGAGGATATTACTGGGCCAGACCCCCC"),
            list("GGTGGAACATTCGAAAGTTGCATAGGGTGGTG"),
            list("GCTCGCGAGTATATTACCGAACCAGCCCCTCA"),
            list("GCAGCTTAGTATGACCGCCAAATCGCGACTCA"),
            list("AGTGGAACCTTGGATAGTCTCATATAGCGGCA"),
            list("GGCGTAATAATCGGATGCTGCAGAGGCTGCTG"),
        ]
        self.assertEqual(part2(sequences), 1245)

    def test_matches(self):
        seq1 = list("CAAGCGCTAAGTTCGCTGGATGTGTGCCCGCG")
        seq2 = list("CTTGAATTGGGCCGTTTACCTGGTTTAACCAT")
        seq3 = list("CTAGCGCTGAGCTGGCTGCCTGGTTGACCGCG")

        self.assertEqual(
            matches(seq1, seq3), self.s2m("+ ++++++ ++ + ++++  ++  ++ +++++")
        )
        self.assertEqual(
            matches(seq2, seq3), self.s2m("++ +   ++ ++ +  + +++++++ + +   ")
        )

    def test_merge(self):
        self.assertEqual(
            merge(
                self.s2m("+ ++++++ ++ + ++++  ++  ++ +++++"),
                self.s2m("++ +   ++ ++ +  + +++++++ + +   "),
            ),
            self.s2m("++++++++++++++++++++++++++++++++"),
        )
        self.assertEqual(
            merge(
                self.s2m("  ++++++ ++ + ++++  ++  ++ +++ +"),
                self.s2m(" + +   ++ ++ +  + +++++++ + +   "),
            ),
            self.s2m(" +++++++++++++++++++++++++++++ +"),
        )


def main():
    failures = 0

    if is_sample:
        failures += check(1, part1(parse("sample1.txt")), 414)
        failures += check(2, part2(parse("sample2.txt")), 1245)
    else:
        failures += check(1, part1(parse("input1.txt")), 6364)
        failures += check(2, part2(parse("input2.txt")), 318598)

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
