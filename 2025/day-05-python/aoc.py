import sys


def part1(ranges, ids):
    return sum(any(lo <= id <= hi for lo, hi in ranges) for id in ids)


def part2(ranges):
    merged = set()

    for lo, hi in ranges:
        overlaps = {
            (c_lo, c_hi)
            for c_lo, c_hi in merged
            if lo <= c_hi and c_lo <= hi or c_lo <= hi and lo <= c_hi
        }

        if overlaps:
            ids = overlaps | {(lo, hi)}
            lo, hi = min(l for l, _ in ids), max(r for _, r in ids)
            merged -= overlaps

        merged.add((lo, hi))

    return sum(hi - lo + 1 for lo, hi in merged)


def parse():
    ranges, ids = open(filename).read().split("\n\n")
    ranges = [
        tuple(map(int, rg.strip().split("-"))) for rg in ranges.strip().split("\n")
    ]
    ids = list(map(int, ids.strip().split("\n")))

    return ranges, ids


def main():
    ranges, ids = parse()

    failures = 0
    failures += check(1, part1(ranges, ids), 3 if is_sample else 509)
    failures += check(2, part2(ranges), 14 if is_sample else 336790092076620)

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
