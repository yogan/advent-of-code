import sys
import unittest


class Fishbone:
    def __init__(self, id, value):
        self.id = id
        self.value = value
        self.left = None
        self.right = None
        self.next = None

    def add(self, value):
        if self.left is None and value < self.value:
            self.left = value
        elif self.right is None and self.value < value:
            self.right = value
        elif self.next:
            self.next.add(value)
        else:
            self.next = Fishbone(None, value)

    def quality(self):
        res = ""
        cur = self
        while cur is not None:
            res += f"{cur.value}"
            cur = cur.next
        return int(res)

    def quality_levels(self):
        res = [self.quality()]
        cur = self
        while cur is not None:
            res.append(
                int(
                    (f"{cur.left}" if cur.left else "")
                    + f"{cur.value}"
                    + (f"{cur.right}" if cur.right else "")
                )
            )
            cur = cur.next
        res.append(self.id)
        return res

    def __lt__(self, other):
        # This works because Python lets us compare arrays in just the way we
        # need it ([3,2,1] > [3,1,2]).
        return self.quality_levels() < other.quality_levels()

    def __repr__(self):
        left = f"{self.left:03}-" if self.left else "    "
        right = f"-{self.right:03}" if self.right else ""
        down = f"\n     |\n{self.next}" if self.next else ""
        return f"{left}{self.value:03}{right}{down}"


def parse(filename):
    swords = []

    for line in open(filename).readlines():
        id, nums = line.split(":")
        swords.append((int(id), list(map(int, nums.split(",")))))

    return swords


def create(id, nums):
    bone = Fishbone(id, nums[0])
    for x in nums[1:]:
        bone.add(x)
    return bone


def part1(nums):
    return create(None, nums).quality()


def part2(swords):
    qualities = [create(*pair).quality() for pair in swords]
    return max(qualities) - min(qualities)


def part3(swords):
    qualities = [(pair[0], create(*pair).quality_levels()) for pair in swords]
    checksum = 0
    for i, q in enumerate(sorted(qualities, key=lambda x: x[1], reverse=True), 1):
        checksum += i * q[0]
    return checksum


class Tests(unittest.TestCase):
    def test_part1(self):
        self.assertEqual(part1([5, 3, 7, 8, 9, 10, 4, 5, 7, 8, 8]), 581078)

    def test_part2(self):
        input = [
            (+1, [2, 4, 1, 1, 8, 2, 7, 9, 8, 6]),
            (+2, [7, 9, 9, 3, 8, 3, 8, 8, 6, 8]),
            (+3, [4, 7, 6, 9, 1, 8, 3, 7, 2, 2]),
            (+4, [6, 4, 2, 1, 7, 4, 5, 5, 5, 8]),
            (+5, [2, 9, 3, 8, 3, 9, 5, 2, 1, 4]),
            (+6, [2, 4, 9, 6, 7, 4, 1, 7, 6, 8]),
            (+7, [2, 3, 7, 6, 2, 2, 4, 1, 4, 2]),
            (+8, [5, 1, 5, 6, 8, 3, 1, 8, 3, 9]),
            (+9, [5, 7, 7, 3, 7, 2, 3, 8, 6, 7]),
            (10, [4, 1, 9, 3, 8, 5, 4, 3, 5, 5]),
        ]
        self.assertEqual(part2(input), 77053)

    def test_quality_levels(self):
        sword1 = create(1, [5, 3, 7, 8, 1, 10, 9, 5, 7, 8])
        self.assertEqual(sword1.quality_levels(), [5897, 357, 1810, 59, 78, 1])

    def test_compare(self):
        sword1 = create(1, [5, 3, 7, 8, 1, 10, 9, 5, 7, 8])
        sword2 = create(2, [5, 3, 7, 8, 1, 10, 9, 4, 7, 9])
        self.assertTrue(sword1 > sword2)
        self.assertTrue(sword2 < sword1)
        self.assertTrue(sword1 != sword2)

    def test_compare_identical(self):
        sword1 = create(1, [7, 1, 9, 1, 6, 9, 8, 3, 7, 2])
        sword2 = create(2, [7, 1, 9, 1, 6, 9, 8, 3, 7, 2])
        self.assertTrue(sword1 < sword2)
        self.assertTrue(sword2 > sword1)
        self.assertTrue(sword1 != sword2)


def main():
    failures = 0

    if is_sample:
        failures += check(1, part1(parse("sample1.txt")[0][1]), 581_078)
        failures += check(2, part2(parse("sample2.txt")), 77_053)
        failures += check(3, part3(parse("sample3.txt")), 260)

    else:
        failures += check(1, part1(parse("input1.txt")[0][1]), 3_753_658_754)
        failures += check(2, part2(parse("input2.txt")), 8_111_665_969_736)
        failures += check(3, part3(parse("input3.txt")), 31_186_361)

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
