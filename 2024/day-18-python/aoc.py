import sys
import unittest


def parse():
    return [
        tuple(int(num) for num in line.strip().split(","))
        for line in open(filename).readlines()
    ]


def shortest_path(memory, bytes, size):
    corrupted = memory[:bytes]
    queue = [((0, 0), 0)]
    seen = set()

    while queue:
        pos, steps = queue.pop(0)
        if pos == (size, size):
            return steps
        if pos in seen:
            continue
        seen.add(pos)
        x, y = pos
        for dx, dy in [(0, 1), (1, 0), (0, -1), (-1, 0)]:
            xx = x + dx
            yy = y + dy
            if xx < 0 or yy < 0 or xx > size or yy > size or (xx, yy) in corrupted:
                continue
            queue.append(((xx, yy), steps + 1))


class Tests(unittest.TestCase):
    pass


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

    memory = parse()
    take = 12 if is_sample else 1024
    size = +6 if is_sample else 70
    part1 = shortest_path(memory, take, size)
    part2 = None

    check(1, part1, 22 if is_sample else 312)
    check(2, part2)
