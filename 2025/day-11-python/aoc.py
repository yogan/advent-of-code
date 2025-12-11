import sys
import unittest
from collections import deque


def part1(graph):
    paths = 0
    seen = set()
    queue = deque([["you"]])

    while queue:
        path = queue.popleft()

        key = tuple(path)
        if key in seen:
            continue
        seen.add(key)

        tail = path[-1]
        if tail == "out":
            paths += 1
            continue

        for node in graph[tail]:
            queue.append(path + [node])

    return paths


def parse():
    graph = dict()
    for line in open(filename).readlines():
        source, targets = line.strip().split(": ")
        graph[source] = targets.split()
    return graph


class Tests(unittest.TestCase):
    pass
    # def test_volume(self):
    #     self.assertEqual(volume([2, 3, 4]), 24)


def main():
    failures = 0
    failures += check(1, part1(parse()), 5 if is_sample else 749)

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
