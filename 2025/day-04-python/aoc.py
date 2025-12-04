import sys


def forklift(grid):
    rows, cols = len(grid), len(grid[0])
    p1, p2 = 0, 0

    while True:
        liftable = [
            (r, c)
            for r in range(rows)
            for c in range(cols)
            if grid[r][c] == "@" and can_lift(grid, rows, cols, r, c)
        ]

        if not liftable:
            return p1, p2

        count = len(liftable)
        p1 = p1 or count
        p2 += count

        for r, c in liftable:
            grid[r][c] = "."


def can_lift(grid, rows, cols, r, c):
    neighbors = [
        (r + dr, c + dc)
        for dr in [-1, 0, 1]
        for dc in [-1, 0, 1]
        if (dr != 0 or dc != 0) and 0 <= r + dr < rows and 0 <= c + dc < cols
    ]
    return sum(grid[nr][nc] == "@" for nr, nc in neighbors) < 4


def parse():
    return [list(line.strip()) for line in open(filename).readlines()]


def main():
    p1, p2 = forklift(parse())

    failures = 0
    failures += check(1, p1, 13 if is_sample else 1551)
    failures += check(2, p2, 43 if is_sample else 9784)

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

    filename = "sample.txt" if is_sample else "input.txt"
    filename = args[0] if args else filename

    main()
