import sys
from collections import defaultdict

file = sys.argv[1] if len(sys.argv) > 1 else "day12.in"
lines = open(file).read().splitlines()
field = [[ord(c) for c in list(line)] for line in lines]
height = len(field)
width = len(field[0])
moves = [(0, 1), (0, -1), (1, 0), (-1, 0)]
start_candidates = []

for i in range(0, height):
    for j in range(0, width):
        if field[i][j] == ord("a"):
            start_candidates.append((i, j))
        if field[i][j] == ord("S"):
            start = (i, j)
            field[i][j] = ord("a")
        elif field[i][j] == ord("E"):
            end = (i, j)
            field[i][j] = ord("z")

def get_reachable(pos):
    i, j = pos
    neighbors = [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)]
    return [n for n in neighbors if n[0] >= 0 and n[0] < height
                                and n[1] >= 0 and n[1] < width
                                and field[n[0]][n[1]] <= field[i][j] + 1]

def find_path(start):
    costs = defaultdict(lambda: float("inf"))
    costs[start] = 0
    stack = []
    stack.append(start)
    while stack:
        pos = stack.pop()
        reachable = get_reachable(pos)
        for n in reachable:
            if costs[pos] + 1 < costs[n]:
                costs[n] = costs[pos] + 1
                stack.append(n)
    return costs[end]

def find_shortest_path():
    min_cost = float("inf")
    for candidate in start_candidates:
        cost = find_path(candidate)
        if cost < min_cost:
            min_cost = cost
    return min_cost

print("Part 1:", find_path(start))
print("Part 2:", find_shortest_path())
