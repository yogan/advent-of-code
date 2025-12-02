import sys
import unittest


def parse():
    return [
        tuple(map(int, id_range.split("-")))
        for line in open(filename).readlines()
        for id_range in line.strip().split(",")
    ]


def part1(ranges):
    return sum_invalid_ids(ranges, part=1)


def part2(ranges):
    return sum_invalid_ids(ranges, part=2)


def sum_invalid_ids(ranges, part):
    total = 0

    for lo, hi in ranges:
        for id in range(lo, hi + 1):
            if part == 1 and repeated_once(str(id)):
                total += id
            elif part == 2 and repeated_seqs(str(id)):
                total += id

    return total


def repeated_once(id):
    m = len(id) // 2
    return id[m:] == id[:m]


def repeated_seqs(id):
    l = len(id)

    for i in range(1, l // 2 + 1):
        seq = id[:i]
        if id == seq * (l // len(seq)):
            return True

    return False


class Tests(unittest.TestCase):
    def test_repeated_once(self):
        self.assertTrue(repeated_once("11"))
        self.assertTrue(repeated_once("1010"))
        self.assertTrue(repeated_once("1188511885"))

        self.assertFalse(repeated_once("10"))
        self.assertFalse(repeated_once("101"))
        self.assertFalse(repeated_once("1188511858"))

    def test_repeated_seqs(self):
        self.assertTrue(repeated_seqs("11"))
        self.assertTrue(repeated_seqs("999"))
        self.assertTrue(repeated_seqs("1010"))
        self.assertTrue(repeated_seqs("1188511885"))

        self.assertFalse(repeated_seqs("12"))
        self.assertFalse(repeated_seqs("998"))
        self.assertFalse(repeated_seqs("10101"))
        self.assertFalse(repeated_seqs("118851188"))


def main():
    failures = 0
    failures += check(1, part1(parse()), 1227775554 if is_sample else 41294979841)
    failures += check(2, part2(parse()), 4174379265 if is_sample else 66500947346)

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
    args = [arg for arg in sys.argv[1:] if not arg.startswith("-")]

    is_sample = "-s" in flags or "--sample" in flags
    run_tests = "-t" in flags or "--test" in flags

    filename = "sample.txt" if is_sample else "input.txt"
    filename = args[0] if args else filename

    if run_tests:
        unittest.main(argv=sys.argv[:1])

    main()
