import sys


def parse():
    return [
        int(line.replace("L", "-").replace("R", ""))
        for line in open(filename).readlines()
    ]


def part1(moves):
    return solve(moves, part=1)


def part2(moves):
    return solve(moves, part=2)


def solve(moves, part):
    total, dial = 0, 50

    for move in moves:
        if part == 1:
            inc = int(dial == 0)
            turn = move
        else:
            if move > 0:
                div, turn = divmod(move, 100)
                inc = div + int(dial + turn >= 100)
            else:
                div, turn = divmod(move, -100)
                inc = div + int(dial != 0 and dial + turn <= 0)

        dial = (dial + turn) % 100
        total += inc

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
