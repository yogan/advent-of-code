import sys
from heapq import heappop, heappush

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
sys.argv  = sys.argv[:1] # strip args, they scare the unittest module
is_sample = filename == "sample.txt"

def parse():
    return [list(map(int, list(x.strip()))) for x in open(filename).readlines()]

def dijkstra(map):
    ROWS, COLS = len(map), len(map[0])
    end = (ROWS - 1, COLS - 1)

    # (heat, row, col, dr, dc, steps)
    queue = [(0, 0, 0, 0, 0, 0)]
    seen = set()

    while queue:
        heat, row, col, cur_dr, cur_dc, steps = heappop(queue)

        if (row, col) == end:
            return heat

        if (row, col, cur_dr, cur_dc, steps) in seen:
            continue

        seen.add((row, col, cur_dr, cur_dc, steps))

        for dr, dc in ((0, 1), (0, -1), (1, 0), (-1, 0)):
            r, c = row + dr, col + dc

            # out of bounds
            if not (0 <= r < ROWS and 0 <= c < COLS):
                continue

            # no moving back where we came from
            if (cur_dr, cur_dc) == (-dr, -dc):
                continue

            # no > 3 steps in the same direction
            same_dir = (cur_dr, cur_dc) == (dr, dc)
            if steps == 3 and same_dir:
                continue

            next_heat = heat + map[r][c]
            next_steps = steps + 1 if same_dir else 1

            heappush(queue, (next_heat, r, c, dr, dc, next_steps))

    assert False, "no path found"

def print_and_assert(part, expected, actual):
    print(f"Part {part}: {actual}{' (sample)' if is_sample else ''}")
    assert actual == expected, f"Part {part} was {actual}, expected {expected}"

if __name__ == '__main__':
    map = parse()

    part1 = dijkstra(map)

    print_and_assert(1, 102 if is_sample else 936, part1)
    # print_and_assert(2, 21756 if is_sample else 4978, part2(lines))
