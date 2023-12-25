import sys
import networkx as nx

if len(sys.argv) == 3 and "--visualize" in sys.argv:
    sys.argv.remove("--visualize")
    visualize = True
else:
    visualize = False
if len(sys.argv) != 2:
    print("Missing input file.")
    exit(1)
filename  = sys.argv[1]
is_sample = filename == "sample.txt"

def split_graph():
    G = parse()

    edges_to_remove = edges_with_max_flow(G, 3)
    assert len(edges_to_remove) == 2 * 3  # G is directed, so edges appear twice

    for edge in edges_to_remove:
        G.remove_edge(*edge)

    (S, T) = get_connected_components(G)
    return S, T

def parse():
    G = nx.DiGraph()
    for line in open(filename).readlines():
        src, targets = line.strip().split(": ")
        for target in targets.split():
            G.add_edge(src, target, capacity=1)
            G.add_edge(target, src, capacity=1)
    return G

def edges_with_max_flow(G, flow):
    edges = []
    for node in G:
        for neighbor in G[node]:
            max_flow = nx.maximum_flow_value(G, node, neighbor)
            if max_flow == flow:
                edges.append((node, neighbor))
    return edges

def get_connected_components(G):
    components = list(nx.connected_components(G.to_undirected()))
    assert len(components) == 2
    return components

def plot(G, S):
    import matplotlib.pyplot as plt

    options = {
        "font_size": 12,
        "node_size": 1000,
        "with_labels": True,
        "font_color": "#505050",
        "node_color": ["#FFB6C1" if node in S else "#ADD8E6" for node in G]
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

if __name__ == '__main__':
    S, T = split_graph()
    part1 = len(S) * len(T)
    check(1, part1, 54 if is_sample else 589036)

    if visualize:
        plot(parse().to_undirected(), S)
