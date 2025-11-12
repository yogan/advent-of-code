import sys
import unittest


def part1(locks, keys, height):
    result = 0
    for lock in locks:
        for key in keys:
            result += all(l + k <= height for l, k in zip(lock, key))
    return result


def parse(blocks):
    height = blocks[0].count("\n") - 1
    empty, full = "." * height, "#" * height
    locks, keys = [], []

    for block in blocks:
        lines = block.strip().split("\n")
        if lines[0] == full and lines[-1] == empty:
            locks.append(heights(lines[1:]))
        elif lines[0] == empty and lines[-1] == full:
            keys.append(heights(lines[:-1]))
        else:
            raise ValueError("Invalid block", block)

    return locks, keys, height


def heights(lines):
    rotated = ["".join(row) for row in zip(*lines)]
    return [row.count("#") for row in rotated]


class Tests(unittest.TestCase):
    def test_parse(self):
        blocks = open("sample.txt").read().split("\n\n")
        locks, keys, _ = parse(blocks)
        self.assertEqual(
            locks,
            [
                [0, 5, 3, 4, 3],
                [1, 2, 0, 5, 3],
            ],
        )
        self.assertEqual(
            keys,
            [
                [5, 0, 2, 1, 3],
                [4, 3, 4, 0, 2],
                [3, 0, 2, 0, 1],
            ],
        )


if __name__ == "__main__":
    flags = set(arg for arg in sys.argv[1:] if arg.startswith("-"))
    args = [arg for arg in sys.argv[1:] if not arg.startswith("-")]

    filename = "sample.txt" if "-s" in flags or "--sample" in flags else "input.txt"
    filename = args[0] if args else filename
    is_sample = filename.startswith("sample")
    run_tests = "-t" in flags or "--test" in flags

    if run_tests:
        unittest.main(argv=sys.argv[:1])

    def check(part, actual, expected=None):
        print(f"Part {part}{' (sample)' if is_sample else ''}: {actual} ", end="")
        if expected is None:
            print("❔")
        else:
            if actual != expected:
                print(f"≠ {expected} ❌")
                exit(1)
            print("✅")

    blocks = open(filename).read().split("\n\n")
    locks, keys, height = parse(blocks)

    check(1, part1(locks, keys, height), 3 if is_sample else 3287)
