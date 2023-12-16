import sys, unittest
from collections import defaultdict

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
sys.argv  = sys.argv[:1] # strip args, they scare the unittest module
is_sample = filename == "sample.txt"

def parse():
    return [list(x.strip()) for x in open(filename).readlines()]

def get_directions(value, dir):
    if value == '.':
        return [dir]

    table = {
        '/':  {'R': ['U'],      'L': ['D'],      'U': ['R'],      'D': ['L']},
        '\\': {'R': ['D'],      'L': ['U'],      'U': ['L'],      'D': ['R']},
        '|':  {'R': ['U', 'D'], 'L': ['U', 'D'], 'U': ['U'],      'D': ['D']},
        '-':  {'R': ['R'],      'L': ['L'],      'U': ['R', 'L'], 'D': ['R', 'L']}
    }

    return table[value][dir]

def move(H, W, row, col, dir):
    if dir == 'R' and col < W - 1:
        return (row, col + 1, dir)
    elif dir == 'L' and col > 0:
        return (row, col - 1, dir)
    elif dir == 'U' and row > 0:
        return (row - 1, col, dir)
    elif dir == 'D' and row < H - 1:
        return (row + 1, col, dir)

    return None

def pewpew(layout):
    H, W = len(layout), len(layout[0])
    start = (0, 0, 'R')
    visited = set()
    visited.add(start)
    queue = [start]

    while queue:
        row, col, dir = queue.pop(0)
        next_dirs = get_directions(layout[row][col], dir)
        for next_dir in next_dirs:
            next = move(H, W, row, col, next_dir)
            if next and next not in visited:
                visited.add(next)
                queue.append(next)

    return set([(row, col) for row, col, _ in visited])

def print_and_assert(part, expected, actual):
    print(f"Part {part}: {actual}{' (sample)' if is_sample else ''}")
    assert actual == expected, f"Part {part} was {actual}, expected {expected}"

if __name__ == '__main__':
    layout = parse()

    energized_tiles = pewpew(layout)
    part1 = len(energized_tiles)

    print_and_assert(1, 46 if is_sample else 8551, part1)
    # print_and_assert(2, 21756 if is_sample else 4978, part2(lines))
