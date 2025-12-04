import sys


def forklift(grid):
    R, C = len(grid), len(grid[0])
    p1, p2 = 0, 0

    while True:
        to_remove = []
        for r in range(R):
            for c in range(C):
                if grid[r][c] == "@" and can_lift(grid, R, C, r, c):
                    to_remove.append((r, c))

        if to_remove:
            rolls = len(to_remove)
            if p1 == 0:
                p1 = rolls
            p2 += rolls
            for r, c in to_remove:
                grid[r][c] = "."
        else:
            return p1, p2


def can_lift(grid, rows, cols, r, c):
    return 4 > (
        sum(
            grid[nr][nc] == "@"
            for nr, nc in [
                (r - 1, c - 1),
                (r - 1, c),
                (r - 1, c + 1),
                (r, c - 1),
                (r, c + 1),
                (r + 1, c - 1),
                (r + 1, c),
                (r + 1, c + 1),
            ]
            if 0 <= nr < rows and 0 <= nc < cols
        )
    )


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
