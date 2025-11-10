import sys
import unittest


class Fishbone:
    def __init__(self, value):
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
            self.next = Fishbone(value)

    def spine(self):
        res = ""
        cur = self
        while cur is not None:
            res += f"{cur.value}"
            cur = cur.next
        return int(res)

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


def part1(nums):
    bone = Fishbone(nums[0])
    for x in nums[1:]:
        bone.add(x)
    return bone.spine()


def part2(swords_with_index):
    qualities = [part1(pair[1]) for pair in swords_with_index]
    return max(qualities) - min(qualities)


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


def main():
    failures = 0

    if is_sample:
        failures += check(1, part1(parse("sample1.txt")[0][1]), 581078)
        failures += check(2, part2(parse("sample2.txt")), 77053)

    else:
        failures += check(1, part1(parse("input1.txt")[0][1]), 3753658754)
        failures += check(2, part2(parse("input2.txt")), 8111665969736)

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
