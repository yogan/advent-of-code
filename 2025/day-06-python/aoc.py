import sys


def part1(lines):
    *rows, ops = (line.strip().split() for line in lines)
    return sum(calc(op, nums) for op, nums in zip(ops, rotate(rows)))


def part2(lines):
    total, op, nums = 0, None, []

    for new_op, *digits in rotate(lines):
        if "".join(digits).strip() == "":
            total += calc(op, nums)
            nums = []
        else:
            op = new_op if new_op != " " else op
            nums.append("".join(reversed(digits)))

    return total


def calc(op, nums):
    return eval(op.join(nums))


def rotate(grid):
    return zip(*grid[::-1])


def parse():
    return open(filename).readlines()


def main():
    failures = 0
    failures += check(1, part1(parse()), 4277556 if is_sample else 8108520669952)
    failures += check(2, part2(parse()), 3263827 if is_sample else 11708563470209)

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
