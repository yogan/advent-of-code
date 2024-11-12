import random
import sys

import networkx as nx

visualize = False
if "--visualize" in sys.argv:
    sys.argv.remove("--visualize")
    visualize = True

fast = False
if "--fast" in sys.argv:
    sys.argv.remove("--fast")
    fast = True

if len(sys.argv) != 2:
    print("Missing input file.")
    exit(1)
filename = sys.argv[1]
is_sample = filename == "sample.txt"


def parse():
    G = nx.DiGraph()
    for line in open(filename).readlines():
        source, targets = line.strip().split(": ")
        for target in targets.split():
            G.add_edge(source, target, capacity=1)
            G.add_edge(target, source, capacity=1)
    return G


def split_graph(G):
    for edge in edges_with_max_flow(G, 3):
        source, target = edge
        G.remove_edge(source, target)
        G.remove_edge(target, source)

    (S, T) = get_connected_components(G)
    return S, T


def edges_with_max_flow(G, flow):
    seen = set()
    edges = []

    for source in G:
        for target in G[source]:
            # skip reverse edges (G is directed, but our wires are not)
            if (target, source) in seen:
                continue
            seen.add((source, target))

            if nx.maximum_flow_value(G, source, target) == flow:
                edges.append((source, target))
                if len(edges) == 3:
                    return edges

    assert False, f"need 3 edges with flow {flow}, only found {len(edges)}"


def get_connected_components(G):
    components = list(nx.connected_components(G.to_undirected()))
    assert len(components) == 2
    return components


def fast_minimum_cut(G):
    while True:
        u = v = random.choice(list(G.nodes))
        while v == u:
            v = random.choice(list(G.nodes))

        cut, partition = nx.minimum_cut(G, u, v)

        if cut == 3:
            (S, T) = partition
            return S, T


def plot(G, S):
    import matplotlib.pyplot as plt

    options = {
        "font_size": 12,
        "node_size": 1000,
        "with_labels": True,
        "font_color": "#505050",
        "node_color": ["#FFB6C1" if node in S else "#ADD8E6" for node in G],
    }

    pos = nx.spring_layout(G, seed=13)  # tried seeds until one was nice
    nx.draw(G, pos=pos, **options)
    plt.show()


def check(part, actual, expected=None):
    print(f"Part {part}{' (sample)' if is_sample else ''}: {actual} ", end="")
    if expected is None:
        print("❔")
    else:
        if actual != expected:
            print(f"≠ {expected} ❌")
            exit(1)
        print("✅")


if __name__ == "__main__":
    G = parse()

    if fast:
        # use nx's minimum_cut (0.3 s)
        S, T = fast_minimum_cut(G)
    else:
        # my original idea (30 s)
        S, T = split_graph(G)

    part1 = len(S) * len(T)

    check(1, part1, 54 if is_sample else 589036)

    if visualize:
        plot(parse().to_undirected(), S)
