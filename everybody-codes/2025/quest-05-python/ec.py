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
    id, nums = open(filename).read().strip().split(":")
    nums = list(map(int, nums.split(",")))
    return id, nums


def part1(nums):
    bone = Fishbone(nums[0])
    for x in nums[1:]:
        bone.add(x)
    return bone.spine()


class Tests(unittest.TestCase):
    def test_part1(self):
        self.assertEqual(part1([5, 3, 7, 8, 9, 10, 4, 5, 7, 8, 8]), 581078)


def main():
    failures = 0

    if is_sample:
        _, nums = parse("sample1.txt")
        failures += check(1, part1(nums), 581078)
    else:
        _, nums = parse("input1.txt")
        failures += check(1, part1(nums), 3753658754)

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
