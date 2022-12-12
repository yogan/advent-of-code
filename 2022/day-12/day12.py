import sys
from collections import defaultdict

file = sys.argv[1] if len(sys.argv) > 1 else "day12.in"
lines = open(file).read().splitlines()
field = [[ord(c) for c in list(line)] for line in lines]
height = len(field)
width = len(field[0])

for i in range(0, height):
    for j in range(0, width):
        if field[i][j] == ord("S"):
            start = (i, j)
            field[i][j] = ord("a")
        elif field[i][j] == ord("E"):
            end = (i, j)
            field[i][j] = ord("z")

costs = defaultdict(lambda: float("inf"))
costs[start] = 0
moves = [(0, 1), (0, -1), (1, 0), (-1, 0)]
path = [start]

def get_reachable(pos):
    i, j = pos
    neighbors = [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)]
    return [n for n in neighbors if n[0] >= 0 and n[0] < height
                                and n[1] >= 0 and n[1] < width
                                and field[n[0]][n[1]] <= field[i][j] + 1]

def find_path():
    stack = []
    stack.append(path[-1])
    while stack:
        pos = stack.pop()
        reachable = get_reachable(pos)
        for n in reachable:
            if costs[pos] + 1 < costs[n]:
                costs[n] = costs[pos] + 1
                path.append(n)
                stack.append(n)

find_path()

print("Part 1:", costs[end])
