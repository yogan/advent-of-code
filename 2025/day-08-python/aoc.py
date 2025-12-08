import math
import sys
import unittest
from collections import defaultdict, deque


def part1(coordinates):
    edge_count = 10 if is_sample else 1000
    graph = connectShortestEdges(coordinates, edge_count)
    components = findLargestComponents(len(coordinates), graph)

    return math.prod(map(len, components))


def connectShortestEdges(coordinates, edge_count):
    n = len(coordinates)
    edges = [
        (distance(coordinates[i], coordinates[j]), i, j)
        for i in range(n)
        for j in range(i + 1, n)
    ]
    edges.sort()

    graph = defaultdict(set)
    for _, i, j in edges[:edge_count]:
        graph[i].add(j)
        graph[j].add(i)

    return graph


def findLargestComponents(nodes, graph):
    visited = set()
    components = []

    for c in range(nodes):
        if c in visited:
            continue
        visited.add(c)

        component = []
        queue = deque([c])

        while queue:
            cur = queue.popleft()
            component.append(cur)

            for neighbor in graph[cur]:
                if neighbor in visited:
                    continue
                visited.add(neighbor)
                queue.append(neighbor)

        components.append(component)

    components.sort(key=len, reverse=True)
    return components[:3]


def distance(p1, p2):
    return math.sqrt(sum((a - b) ** 2 for a, b in zip(p1, p2)))


def parse():
    return [tuple(map(int, l.strip().split(","))) for l in open(filename).readlines()]


class Tests(unittest.TestCase):
    def test_distance(self):
        self.assertAlmostEqual(distance((162, 817, 812), (425, 690, 689)), 316.9, 1)


def main():
    failures = 0
    failures += check(1, part1(parse()), 40 if is_sample else 62186)

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
