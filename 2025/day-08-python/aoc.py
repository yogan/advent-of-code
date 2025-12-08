import math
import sys
import unittest
from collections import defaultdict, deque


def solve(coordinates):
    edge_count = 10 if is_sample else 1000
    edges, graph = connectShortestEdges(coordinates, edge_count)
    p1 = part1(coordinates, graph)
    p2 = part2(coordinates, edges)
    return p1, p2


def part1(coordinates, graph):
    components = findLargestComponents(len(coordinates), graph)
    return math.prod(map(len, components))


def part2(coordinates, edges):
    (x1, _, _), (x2, _, _) = connectAll(coordinates, edges)  # type: ignore
    return x1 * x2


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

    return edges, graph


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


def connectAll(nodes, edges):
    components = []

    for _, i, j in edges:
        i_comp = [comp for comp in components if i in comp]
        j_comp = [comp for comp in components if j in comp]
        if not i_comp and not j_comp:
            components.append(set([i, j]))
        elif i_comp and j_comp:
            if i_comp == j_comp:
                continue
            components.remove(i_comp[0])
            components.remove(j_comp[0])
            components.append(i_comp[0] | j_comp[0])
        elif i_comp:
            i_comp[0].add(j)
        elif j_comp:
            j_comp[0].add(i)

        if len(components) == 1 and len(components[0]) == len(nodes):
            return nodes[i], nodes[j]


def distance(p1, p2):
    return math.sqrt(sum((a - b) ** 2 for a, b in zip(p1, p2)))


def parse():
    return [tuple(map(int, l.strip().split(","))) for l in open(filename).readlines()]


class Tests(unittest.TestCase):
    def test_distance(self):
        self.assertAlmostEqual(distance((162, 817, 812), (425, 690, 689)), 316.9, 1)


def main():
    p1, p2 = solve(parse())

    failures = 0
    failures += check(1, p1, 40 if is_sample else 62186)
    failures += check(2, p2, 25272 if is_sample else 8420405530)

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
