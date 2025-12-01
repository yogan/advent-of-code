import sys


def parse():
    return [(line[0], int(line[1:])) for line in open(filename).readlines()]


def part1(moves):
    total = 0
    dial = 50

    for d, dist in moves:
        if d == "R":
            dial = (dial + dist) % 100
        else:
            dial = (dial - dist) % 100
        if dial == 0:
            total += 1

    return total


def part2(moves):
    total = 0
    dial = 50

    for d, dist in moves:
        total += dist // 100
        dist %= 100

        if d == "R":
            dial = dial + dist
            if dial >= 100:
                total += 1
        else:
            prev = dial
            dial = dial - dist
            if prev != 0 and dial <= 0:
                total += 1

        dial %= 100

    return total


def main():
    failures = 0
    failures += check(1, part1(parse()), 3 if is_sample else 1064)
    failures += check(2, part2(parse()), 6 if is_sample else 6122)

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
