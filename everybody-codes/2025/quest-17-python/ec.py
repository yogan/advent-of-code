import sys
from heapq import heappop, heappush


def parse(filename):
    start, volcano = None, None
    grid = []
    for row, line in enumerate(open(filename).readlines()):
        col = line.find("@")
        if col != -1:
            volcano = (row, col)
        col = line.find("S")
        if col != -1:
            start = (row, col)
        grid.append(list(map(int, line.replace("S", "0").replace("@", "0").strip())))
    return grid, volcano, start


def part1(grid, volcano, _):
    return sum(
        grid[r][c]
        for r in range(len(grid))
        for c in range(len(grid[0]))
        if in_circle(volcano[0] - r, volcano[1] - c, 10)
    )


def part2(grid, volcano, _):
    vr, vc = volcano
    best, best_dist = 0, 0
    seen = set()

    for dist in range(1, vr + 1):
        border = set(
            (r, c)
            for r in range(len(grid))
            for c in range(len(grid[0]))
            if in_circle(vr - r, vc - c, dist) and not (r, c) in seen
        )
        seen |= border
        cur = sum(grid[r][c] for r, c in border)
        if cur > best:
            best = cur
            best_dist = dist

    return best * best_dist


def part3(grid, volcano, start):
    rows, cols = len(grid), len(grid[0])
    vr, vc = volcano

    for radius in range(rows):
        # mark volcano cells with None so that `dijkstra_loop()` won't enter them
        for r in range(rows):
            for c in range(cols):
                if in_circle(vr - r, vc - c, radius):
                    grid[r][c] = None

        time = dijkstra_loop(grid, volcano, start)
        time_available = 30 * (radius + 1)
        if time and time < time_available:
            return time * radius


def dijkstra_loop(grid, volcano, start):
    rows, cols = len(grid), len(grid[0])
    sr, sc = start
    vr, vc = volcano

    queue = [(0, sr, sc, False)]
    times = {}

    while queue:
        time, r, c, crossed = heappop(queue)

        if r == sr and c == sc and crossed:
            return time

        state = (r, c, crossed)
        if state in times and times[state] <= time:
            continue
        times[state] = time

        for nr, nc in [(r, c + 1), (r, c - 1), (r + 1, c), (r - 1, c)]:
            # None on grid means the volcano has already destroyed this cell
            if 0 <= nr < rows and 0 <= nc < cols and grid[nr][nc] is not None:
                n_time = time + grid[nr][nc]
                n_crossed = crossed ^ crosses_barrier(r, c, nr, nc, vr, vc)
                heappush(queue, (n_time, nr, nc, n_crossed))


def crosses_barrier(r, c, nr, nc, vr, vc):
    return (r > vr and nr > vr) and ((c <= vc) != (nc <= vc))


def in_circle(dr, dc, r):
    return dr * dr + dc * dc <= r * r


def main():
    failures = 0

    if is_sample:
        failures += check(1, part1(*parse("sample1.txt")), 1573)
        failures += check(2, part2(*parse("sample2.txt")), 1090)
        failures += check("3a", part3(*parse("sample3a.txt")), 592)
        failures += check("3b", part3(*parse("sample3b.txt")), 330)
        failures += check("3c", part3(*parse("sample3c.txt")), 3180)
    else:
        failures += check(1, part1(*parse("input1.txt")), 1547)
        failures += check(2, part2(*parse("input2.txt")), 66222)
        failures += check(3, part3(*parse("input3.txt")), 48680)

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
    is_sample = "-s" in flags or "--sample" in flags
    main()
