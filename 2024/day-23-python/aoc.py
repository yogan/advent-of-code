import sys


def parse(filename):
    E = []
    V = set()
    for line in open(filename).readlines():
        a, b = line.strip().split("-")
        E.append((a, b))
        E.append((b, a))
        V |= {a, b}
    return E, list(V)


def solve(E, V):
    three_cliques, max_cliques = [], []
    bron_kerbosch(set(), set(V), set(), E, three_cliques, max_cliques)

    p1 = sum(any(node.startswith("t") for node in c) for c in three_cliques)
    p2 = ",".join(sorted(max(max_cliques, key=len)))

    return p1, p2


def bron_kerbosch(R, P, X, E, three_cliques, max_cliques):
    def neighbors(E, v):
        return set(b for a, b in E if a == v) | set(a for a, b in E if b == v)

    if len(R) == 3:
        three_cliques.append(R)

    if not P and not X:
        max_cliques.append(R)
        return

    for v in P.copy():
        N = neighbors(E, v)
        bron_kerbosch(R | {v}, P & N, X & N, E, three_cliques, max_cliques)
        P.remove(v)
        X.add(v)


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

    p1, p2 = solve(*parse(filename))
    p2_exp = "co,de,ka,ta" if is_sample else "cl,df,ft,ir,iy,ny,qp,rb,sh,sl,sw,wm,wy"
    check(1, p1, 7 if is_sample else 1227)
    check(2, p2, p2_exp)
