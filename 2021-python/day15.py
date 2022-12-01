from collections import defaultdict
import heapq
import unittest
from input import read_and_solve


def parse_input(lines):
    graph = {}
    for i, line in enumerate(lines):
        for j, char in enumerate(line):
            graph[(i, j)] = int(char)
    return graph


def extend(graph):
    bottom_right = max(graph.keys())
    x_max = bottom_right[0] + 1
    y_max = bottom_right[1] + 1

    for x in range(5 * x_max):
        for y in range(5 * y_max):
            if x < x_max and y < y_max:
                continue
            value = graph[(x % x_max, y % y_max)] \
                + (x // x_max) + (y // y_max)
            graph[(x, y)] = value if value < 10 else value % 10 + 1


def dijkstra(graph):
    x_max, y_max = max(graph.keys())

    dx = [0, 1, 0, -1]
    dy = [1, 0, -1, 0]

    costs = defaultdict(lambda: None)

    queue = []
    heapq.heappush(queue, (0, 0, 0))

    while queue:
        cost, x, y = heapq.heappop(queue)

        if costs[(x, y)] is not None:
            continue

        new_cost = cost + graph[(x, y)]
        costs[(x, y)] = new_cost

        for d in range(len(dx)):
            xx = x + dx[d]
            yy = y + dy[d]
            if xx >= 0 and yy >= 0 and xx <= x_max and yy <= y_max:
                heapq.heappush(queue, (new_cost, xx, yy))

    return costs[(x_max, y_max)] - costs[(0, 0)]


def part1(lines):
    cave = parse_input(lines)
    return dijkstra(cave)


def part2(lines):
    cave = parse_input(lines)
    extend(cave)
    return dijkstra(cave)


class TestDay15(unittest.TestCase):

    sample = [
        "1163751742",
        "1381373672",
        "2136511328",
        "3694931569",
        "7463417111",
        "1319128137",
        "1359912421",
        "3125421639",
        "1293138521",
        "2311944581",
    ]

    def test_parse_input(self):
        cave = parse_input(["123", "432"])
        self.assertEqual(cave, {
            (0, 0): 1, (0, 1): 2, (0, 2): 3,
            (1, 0): 4, (1, 1): 3, (1, 2): 2,
        })

    def test_extend(self):
        cave = {(0, 0): 8}
        extend(cave)
        self.assertEqual(cave, {
            (0, 0): 8, (0, 1): 9, (0, 2): 1, (0, 3): 2, (0, 4): 3,
            (1, 0): 9, (1, 1): 1, (1, 2): 2, (1, 3): 3, (1, 4): 4,
            (2, 0): 1, (2, 1): 2, (2, 2): 3, (2, 3): 4, (2, 4): 5,
            (3, 0): 2, (3, 1): 3, (3, 2): 4, (3, 3): 5, (3, 4): 6,
            (4, 0): 3, (4, 1): 4, (4, 2): 5, (4, 3): 6, (4, 4): 7,
        })

    def test_part_1_sample(self):
        self.assertEqual(part1(self.sample), 40)

    def test_part_2_sample(self):
        self.assertEqual(part2(self.sample), 315)


if __name__ == '__main__':
    unittest.main(exit=False)
    read_and_solve(__file__, part1, part2)
