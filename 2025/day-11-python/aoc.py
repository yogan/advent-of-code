import sys
from collections import deque


def part1(graph):
    return count_paths(graph, "you", "out")


def part2(graph):
    svr_to_fft = count_paths(graph, "svr", "fft")
    fft_to_dac = count_paths(graph, "fft", "dac")
    dac_to_out = count_paths(graph, "dac", "out")

    return svr_to_fft * fft_to_dac * dac_to_out


def count_paths(graph, start, end, memo=None):
    if memo is None:
        memo = {}

    if start in memo:
        return memo[start]

    if start == end:
        return 1

    paths = 0
    if start in graph:
        for neighbor in graph[start]:
            paths += count_paths(graph, neighbor, end, memo)

    memo[start] = paths
    return paths


def parse(filename):
    graph = dict()
    for line in open(filename).readlines():
        source, targets = line.strip().split(": ")
        graph[source] = targets.split()
    return graph


def main():
    if is_sample:
        p1 = part1(parse("sample1.txt"))
        p2 = part2(parse("sample2.txt"))
    else:
        graph = parse("input.txt")
        p1 = part1(graph)
        p2 = part2(graph)

    failures = 0
    failures += check(1, p1, 5 if is_sample else 749)
    failures += check(2, p2, 2 if is_sample else 420257875695750)

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

    main()
