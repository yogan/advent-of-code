import math
import sys


def solve(nodes):
    n = len(nodes)
    edges = sorted(
        (dist(nodes[i], nodes[j]), i, j) for i in range(n) for j in range(i + 1, n)
    )

    p1, p2, components = None, None, []

    for connections, (_, i, j) in enumerate(edges):
        i_comp = next((comp for comp in components if i in comp), None)
        j_comp = next((comp for comp in components if j in comp), None)

        if not i_comp and not j_comp:
            components.append({i, j})
        elif i_comp and j_comp:
            if i_comp is j_comp:
                continue
            components.remove(i_comp)
            components.remove(j_comp)
            components.append(i_comp | j_comp)
        elif i_comp:
            i_comp.add(j)
        elif j_comp:
            j_comp.add(i)

        if connections == (9 if is_sample else 999):
            p1 = math.prod(map(len, sorted(components, key=len)[-3:]))
        elif len(components[0]) == n:
            (x1, _, _), (x2, _, _) = nodes[i], nodes[j]
            p2 = x1 * x2
            break

    return p1, p2


def dist(a, b):
    return sum((a - b) ** 2 for a, b in zip(a, b))


def parse():
    return [tuple(map(int, l.strip().split(","))) for l in open(filename).readlines()]


def main():
    p1, p2 = solve(parse())

    failures = 0
    failures += check(1, p1, 40 if is_sample else 62186)
    failures += check(2, p2, 25272 if is_sample else 8420405530)

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
