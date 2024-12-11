import sys
import unittest


def parse(filename):
    return list(map(int, open(filename).read().split()))


def part1(stones):
    for _ in range(25):
        stones = blink(stones)
    return len(stones)


def blink(stones):
    new_stones = []
    for stone in stones:
        stone_str = str(stone)
        if stone == 0:
            new_stones.append(1)
        elif len(stone_str) % 2 == 0:
            mid = len(stone_str) // 2
            new_stones.append(int(stone_str[:mid]))
            new_stones.append(int(stone_str[mid:]))
        else:
            new_stones.append(stone * 2024)

    return new_stones


class Tests(unittest.TestCase):
    def test_blink(self):
        self.assertEqual(blink([0, 1, 10, 99, 999]), [1, 2024, 1, 0, 9, 9, 2021976])


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

    stones = parse(filename)
    p1 = part1(stones)
    p2 = None

    check(1, p1, 55312 if is_sample else 233050)
    check(2, p2)
