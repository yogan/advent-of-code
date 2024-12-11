import sys
import unittest


def parse(filename):
    dict = {}
    for num_str in open(filename).read().split():
        num = int(num_str)
        if num in dict:
            dict[num] += 1
        else:
            dict[num] = 1
    return dict


def blinkblink(stones, times):
    for _ in range(times):
        stones = blink(stones)
    return sum(stones.values())


def blink(stones):
    new_stones = {}

    for stone, factor in stones.items():
        for new_stone in evolve(stone):
            if new_stone in new_stones:
                new_stones[new_stone] += factor
            else:
                new_stones[new_stone] = factor

    return new_stones


def evolve(stone):
    if stone == 0:
        return [1]

    stone_str = str(stone)
    if len(stone_str) % 2 == 0:
        mid = len(stone_str) // 2
        return [int(stone_str[:mid]), int(stone_str[mid:])]

    return [stone * 2024]


class Tests(unittest.TestCase):
    def test_blink(self):
        self.assertEqual(
            blink({0: 1, 1: 1, 10: 1, 99: 1, 999: 1}),
            {1: 2, 2024: 1, 0: 1, 9: 2, 2021976: 1},
        )


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
    p1 = blinkblink(stones, 25)
    p2 = blinkblink(stones, 75)

    check(1, p1, 55312 if is_sample else 233050)
    check(2, p2, 65601038650482 if is_sample else 276661131175807)
