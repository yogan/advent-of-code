import sys, unittest
from collections import defaultdict
from enum import Enum

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
sys.argv  = sys.argv[:1] # strip args, they scare the unittest module
is_sample = filename == "sample.txt"

def parse():
    with open(filename) as f:
        pipes = [list(x.strip()) for x in f.readlines()]
    W, H = len(pipes[0]), len(pipes)
    for x in range(W):
        for y in range(H):
            if pipes[y][x] == 'S':
                pos = (x, y)
    return (pipes, W, H, pos)

def connected_start_neighbors(pipes, W, H, pos):
    x, y = pos
    assert pipes[y][x] == 'S'

    neighbors = []

    if x > 0 and pipes[y][x-1] in ['-', 'F', 'L']:
        neighbors.append((x-1, y))
    if x < W-1 and pipes[y][x+1] in ['-', '7', 'J']:
        neighbors.append((x+1, y))
    if y > 0 and pipes[y-1][x] in ['|', 'F', '7']:
        neighbors.append((x, y-1))
    if y < H-1 and pipes[y+1][x] in ['|', 'L', 'J']:
        neighbors.append((x, y+1))

    assert len(neighbors) == 2, f"Start has != 2 neighbors: {neighbors}"

    return neighbors

def connected_neighbors(pipes, W, H, pos):
    x, y = pos
    cur = pipes[y][x]
    assert cur != 'S'

    def add_if_top():
        if y > 0 and pipes[y-1][x] in ['|', 'F', '7', 'S']:
            neighbors.append((x, y-1))

    def add_if_bottom():
        if y < H-1 and pipes[y+1][x] in ['|', 'L', 'J', 'S']:
            neighbors.append((x, y+1))

    def add_if_left():
        if x > 0 and pipes[y][x-1] in ['-', 'F', 'L', 'S']:
            neighbors.append((x-1, y))

    def add_if_right():
        if x < W-1 and pipes[y][x+1] in ['-', '7', 'J', 'S']:
            neighbors.append((x+1, y))

    neighbors = []

    if cur == '|':
        add_if_top()
        add_if_bottom()
    elif cur == '-':
        add_if_left()
        add_if_right()
    elif cur == 'L':
        add_if_top()
        add_if_right()
    elif cur == 'J':
        add_if_top()
        add_if_left()
    elif cur == '7':
        add_if_left()
        add_if_bottom()
    elif cur == 'F':
        add_if_right()
        add_if_bottom()
    else:
        assert False, f"Unknown pipe type: {cur}"

    assert len(neighbors) == 2, f"{pos} ({pipes[y][x]}) != 2 neighbors: {neighbors}"

    return neighbors

def walk(pipes, W, H, start):
    start_neighbors = connected_start_neighbors(pipes, W, H, start)

    prev_pos = start
    pos = start_neighbors[0]
    path = [pos]
    while pos != start:
        neighbors = connected_neighbors(pipes, W, H, pos)

        next_positions = [x for x in neighbors if x != prev_pos]
        assert len(next_positions) == 1, f"{pos} has > 1 next positions: {next_positions}"

        prev_pos = pos
        pos = next_positions[0]
        assert pos not in path, f"{pos} already in path"
        path.append(pos)

    assert len(path) % 2 == 0, f"Path has odd length: {path}"
    return len(path) // 2

class TestDay10(unittest.TestCase):
    pass

if __name__ == '__main__':
    unittest.main(exit=False)
    print()
    # exit(1)

    pipes, W, H, start = parse()

    res1 = walk(pipes, W, H, start)
    assert res1 == (4 if is_sample else 6867)
    print(f"Part 1: {res1}{' (sample)' if is_sample else ''}")

    res2 = None
    assert res2 == (None if is_sample else None)
    print(f"Part 2: {res2}{' (sample)' if is_sample else ''}")
