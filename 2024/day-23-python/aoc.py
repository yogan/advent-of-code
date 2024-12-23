import sys
from collections import defaultdict

G = defaultdict(set)
three_cliques, max_cliques = [], []


def parse():
    for line in open(filename).readlines():
        a, b = line.strip().split("-")
        G[a].add(b)
        G[b].add(a)


def solve():
    bron_kerbosch(set(), set(G), set())

    p1 = sum(any(node.startswith("t") for node in c) for c in three_cliques)
    p2 = ",".join(sorted(max(max_cliques, key=len)))

    return p1, p2


def bron_kerbosch(R, P, X):
    if len(R) == 3:
        three_cliques.append(R)

    if not P and not X:
        max_cliques.append(R)
        return

    for v in P.copy():
        N = G[v]
        bron_kerbosch(R | {v}, P & N, X & N)
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

    parse()
    p1, p2 = solve()
    p2_exp = "co,de,ka,ta" if is_sample else "cl,df,ft,ir,iy,ny,qp,rb,sh,sl,sw,wm,wy"
    check(1, p1, 7 if is_sample else 1227)
    check(2, p2, p2_exp)
