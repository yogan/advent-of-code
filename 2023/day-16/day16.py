import sys
from collections import defaultdict

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
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

def pewpew(layout, start):
    H, W = len(layout), len(layout[0])
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

    return len(set([(row, col) for row, col, _ in visited]))

def start_positions(layout):
    H, W = len(layout), len(layout[0])

    top_row =    [(    0, col,   'D') for col in range(W)]
    bottom_row = [(H - 1, col,   'U') for col in range(W)]
    left_col =   [(  row, 0,     'R') for row in range(H)]
    right_col =  [(  row, W - 1, 'L') for row in range(H)]

    # starting with left_col so that part1 start is the first entry
    return [*left_col, *top_row, *bottom_row, *right_col]

def print_and_assert(part, expected, actual):
    print(f"Part {part}: {actual}{' (sample)' if is_sample else ''}")
    assert actual == expected, f"Part {part} was {actual}, expected {expected}"

if __name__ == '__main__':
    layout = parse()
    starts = start_positions(layout)

    energy_levels = [pewpew(layout, start) for start in starts]
    part1 = energy_levels[0]
    part2 = max(energy_levels)

    print_and_assert(1, 46 if is_sample else 8551, part1)
    print_and_assert(2, 51 if is_sample else 8754, part2)
