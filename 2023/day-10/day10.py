import sys, unittest
from collections import defaultdict
from enum import Enum

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
sys.argv  = sys.argv[:1] # strip args, they scare the unittest module
is_sample = filename.startswith('sample')
expected1 = {
    "input.txt": 6867,
    "sample1.txt": 4,
    "sample2.txt": 23,
    "sample3.txt": 22,
    "sample4.txt": 80,
}
expected2 = {
    "input.txt": None,
    "sample1.txt": None,
    "sample2.txt": None,
    "sample3.txt": None,
    "sample4.txt": None,
}

def parse():
    with open(filename) as f:
        grid = [list(x.strip()) for x in f.readlines()]
    W, H = len(grid[0]), len(grid)
    for x in range(W):
        for y in range(H):
            if grid[y][x] == 'S':
                pos = (x, y)
    return (grid, W, H, pos)

class Colors:
    RESET   = '\033[0m'
    BLACK   = '\033[30m'
    RED     = '\033[31m'
    GREEN   = '\033[32m'
    YELLOW  = '\033[33m'
    BLUE    = '\033[34m'
    MAGENTA = '\033[35m'
    CYAN    = '\033[36m'
    WHITE   = '\033[37m'

    BG_RESET   = '\033[49m'
    BG_BLACK   = '\033[40m'
    BG_RED     = '\033[41m'
    BG_GREEN   = '\033[42m'
    BG_YELLOW  = '\033[43m'
    BG_BLUE    = '\033[44m'
    BG_MAGENTA = '\033[45m'
    BG_CYAN    = '\033[46m'
    BG_WHITE   = '\033[47m'

def print_sketch(grid, path):
    for y, line in enumerate(grid):
        for x, c in enumerate(line):
            if (x, y) in path:
                print(Colors.MAGENTA + Colors.BG_BLACK, end='')
                if c == 'S':
                    print(Colors.GREEN + '×', end='')
                elif c == '|':
                    print('│', end='')
                elif c == '-':
                    print('─', end='')
                elif c == 'L':
                    print('└', end='')
                elif c == 'J':
                    print('┘', end='')
                elif c == '7':
                    print('┐', end='')
                elif c == 'F':
                    print('┌', end='')
            else:
                # print(Colors.RESET + Colors.BG_RESET + ' ', end='')
                print(Colors.RESET + Colors.BG_RESET + '·', end='')
        print()
    print(Colors.RESET + Colors.BG_RESET)

def connected_start_neighbors(grid, W, H, pos):
    x, y = pos
    assert grid[y][x] == 'S'

    neighbors = []

    if x > 0 and grid[y][x-1] in ['-', 'F', 'L']:
        neighbors.append((x-1, y))
    if x < W-1 and grid[y][x+1] in ['-', '7', 'J']:
        neighbors.append((x+1, y))
    if y > 0 and grid[y-1][x] in ['|', 'F', '7']:
        neighbors.append((x, y-1))
    if y < H-1 and grid[y+1][x] in ['|', 'L', 'J']:
        neighbors.append((x, y+1))

    assert len(neighbors) == 2, f"Start has != 2 neighbors: {neighbors}"

    return neighbors

def connected_neighbors(grid, W, H, pos):
    x, y = pos
    cur = grid[y][x]
    assert cur != 'S'

    def add_if_top():
        if y > 0 and grid[y-1][x] in ['|', 'F', '7', 'S']:
            neighbors.append((x, y-1))

    def add_if_bottom():
        if y < H-1 and grid[y+1][x] in ['|', 'L', 'J', 'S']:
            neighbors.append((x, y+1))

    def add_if_left():
        if x > 0 and grid[y][x-1] in ['-', 'F', 'L', 'S']:
            neighbors.append((x-1, y))

    def add_if_right():
        if x < W-1 and grid[y][x+1] in ['-', '7', 'J', 'S']:
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

    assert len(neighbors) == 2, f"{pos} ({grid[y][x]}) != 2 neighbors: {neighbors}"

    return neighbors

def walk(grid, W, H, start):
    start_neighbors = connected_start_neighbors(grid, W, H, start)

    prev_pos = start
    pos = start_neighbors[0]
    path = [pos]
    while pos != start:
        neighbors = connected_neighbors(grid, W, H, pos)

        next_positions = [x for x in neighbors if x != prev_pos]
        assert len(next_positions) == 1, f"{pos} has > 1 next positions: {next_positions}"

        prev_pos = pos
        pos = next_positions[0]
        assert pos not in path, f"{pos} already in path"
        path.append(pos)

    return path

def zoom_in(grid, W, H, start, path):
    big_grid = []
    for y in range(H*2):
        big_grid.append(['.'] * W*2)

    big_path = []

    big_start = (start[0]*2, start[1]*2)
    big_grid[big_start[0]][big_start[1]] = 'S'

    last = start

    for pos in path:
        x, y = pos
        big_x, big_y = x*2, y*2
        last_x, last_y = last

        if last_x < x:
            # last to pos was left to right
            assert last_y == y and last_x == x-1
            left_big_x, left_big_y = big_x-1, big_y
            big_grid[left_big_y][left_big_x] = '-'
            big_path.append((left_big_x, left_big_y))
        elif last_x > x:
            # last to pos was right to left
            assert last_y == y and last_x == x+1
            right_big_x, right_big_y = big_x+1, big_y
            big_grid[right_big_y][right_big_x] = '-'
            big_path.append((right_big_x, right_big_y))
        elif last_y < y:
            # last to pos was top to bottom
            assert last_x == x and last_y == y-1
            above_big_x, above_big_y = big_x, big_y-1
            big_grid[above_big_y][above_big_x] = '|'
            big_path.append((above_big_x, above_big_y))
        elif last_y > y:
            # last to pos was bottom to top
            assert last_x == x and last_y == y+1
            below_big_x, below_big_y = big_x, big_y+1
            big_grid[below_big_y][below_big_x] = '|'
            big_path.append((below_big_x, below_big_y))
        else:
            assert False, f"Last pos {last} == cur pos {pos}"

        big_grid[big_y][big_x] = grid[y][x]
        big_path.append((big_x, big_y))

        last = pos

    return big_grid, big_path

def part1(path):
    assert len(path) % 2 == 0, f"Path has odd length: {path}"
    return len(path) // 2

def print_and_assert(part, expected, actual):
    print(f"{part}: {actual}{' (sample)' if is_sample else ''}")
    assert actual == expected, f"{part} was {actual}, expected {expected}"

if __name__ == '__main__':
    grid, W, H, start = parse()
    path = walk(grid, W, H, start)
    print_sketch(grid, path)

    big_grid, big_path = zoom_in(grid, W, H, start, path)
    print_sketch(big_grid, big_path)

    print_and_assert("Part 1", expected1[filename], part1(path))
    print_and_assert("Part 2", expected2[filename], None)
