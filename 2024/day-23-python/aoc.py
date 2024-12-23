import sys


def parse(filename):
    E = []
    V = set()
    for line in open(filename).readlines():
        a, b = line.strip().split("-")
        E.append((a, b))
        E.append((b, a))
        V.add(a)
        V.add(b)
    return E, list(V)


def find_3_cliques(E, V):
    cliques = set()
    for a, b in E:
        for c in V:
            if (b, c) in E and (c, a) in E:
                if (
                    (a, b, c) not in cliques
                    and (a, c, b) not in cliques
                    and (b, a, c) not in cliques
                    and (b, c, a) not in cliques
                    and (c, a, b) not in cliques
                    and (c, b, a) not in cliques
                    and (a[0] == "t" or b[0] == "t" or c[0] == "t")
                ):
                    cliques.add((a, b, c))
    return cliques


if __name__ == "__main__":
    flags = set(arg for arg in sys.argv[1:] if arg.startswith("-"))
    args = [arg for arg in sys.argv[1:] if not arg.startswith("-")]

    filename = "sample.txt" if "-s" in flags or "--sample" in flags else "input.txt"
    filename = args[0] if args else filename
    is_sample = filename.startswith("sample")

    def check(part, actual, expected=None):
        print(f"Part {part}{' (sample)' if is_sample else ''}: {actual} ", end="")
        if expected is None:
            print("❔")
        else:
            if actual != expected:
                print(f"≠ {expected} ❌")
                exit(1)
            print("✅")

    E, V = parse(filename)
    cliques = find_3_cliques(E, V)

    part1 = len(cliques)
    part2 = None

    check(1, part1, 7 if is_sample else 1227)
    check(2, part2)
