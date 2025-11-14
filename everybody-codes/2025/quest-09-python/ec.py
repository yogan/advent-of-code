import sys
import unittest


def parse(filename):
    return [to_bitmap(l.strip().split(":")[1]) for l in open(filename).readlines()]


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


def part3(sequences):
    children = set()
    families = []

    for i in range(len(sequences)):
        for j in range(i + 1, len(sequences)):
            for ci, child in enumerate(sequences):
                if ci in [i, j] or ci in children:
                    continue
                if score(sequences[i], sequences[j], child):
                    children.add(ci)
                    families.append(set([i + 1, j + 1, ci + 1]))

    combined = []

    for fam in families:
        for connected in [comb for comb in combined if comb & fam]:
            combined.remove(connected)
            fam |= connected
        combined.append(fam)

    return sum(sorted(combined, key=lambda f: -len(f))[0])


def to_bitmap(seq):
    map = {"A": 0b0001, "T": 0b0010, "G": 0b0100, "C": 0b1000}
    num = 0
    for c in seq:
        num <<= 4
        num |= map.get(c, 0)
    return num


def score(p1, p2, c):
    m1 = p1 & c
    m2 = p2 & c

    return m1.bit_count() * m2.bit_count() if m1 | m2 == c else 0


class Tests(unittest.TestCase):
    def test_part1(self):
        seq1 = to_bitmap(list("CAAGCGCTAAGTTCGCTGGATGTGTGCCCGCG"))
        seq2 = to_bitmap(list("CTTGAATTGGGCCGTTTACCTGGTTTAACCAT"))
        seq3 = to_bitmap(list("CTAGCGCTGAGCTGGCTGCCTGGTTGACCGCG"))
        self.assertEqual(part1([seq1, seq2, seq3]), 414)

    def test_part2(self):
        sequences = [
            to_bitmap(list("GCAGGCGAGTATGATACCCGGCTAGCCACCCC")),
            to_bitmap(list("TCTCGCGAGGATATTACTGGGCCAGACCCCCC")),
            to_bitmap(list("GGTGGAACATTCGAAAGTTGCATAGGGTGGTG")),
            to_bitmap(list("GCTCGCGAGTATATTACCGAACCAGCCCCTCA")),
            to_bitmap(list("GCAGCTTAGTATGACCGCCAAATCGCGACTCA")),
            to_bitmap(list("AGTGGAACCTTGGATAGTCTCATATAGCGGCA")),
            to_bitmap(list("GGCGTAATAATCGGATGCTGCAGAGGCTGCTG")),
        ]
        self.assertEqual(part2(sequences), 1245)

    def test_score(self):
        seq1 = to_bitmap(list("CAAGCGCTAAGTTCGCTGGATGTGTGCCCGCG"))
        seq2 = to_bitmap(list("CTTGAATTGGGCCGTTTACCTGGTTTAACCAT"))
        seq3 = to_bitmap(list("CTAGCGCTGAGCTGGCTGCCTGGTTGACCGCG"))
        seq4 = to_bitmap(list("ATAGCGCTGAGCTGGCTGCCTGGTTGACCGCG"))
        self.assertEqual(score(seq1, seq2, seq3), 414)
        self.assertEqual(score(seq1, seq2, seq4), 0)


def main():
    failures = 0

    if is_sample:
        failures += check(1, part1(parse("sample1.txt")), 414)
        failures += check(2, part2(parse("sample2.txt")), 1245)
        failures += check("3a", part3(parse("sample3a.txt")), 12)
        failures += check("3b", part3(parse("sample3b.txt")), 36)
    else:
        failures += check(1, part1(parse("input1.txt")), 6364)
        failures += check(2, part2(parse("input2.txt")), 318598)
        failures += check(3, part3(parse("input3.txt")), 38604)

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
