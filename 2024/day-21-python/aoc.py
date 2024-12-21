import sys
import unittest

DIRECTIONAL = {
    # ^
    ("^", "^"): [""],
    ("^", "A"): [">"],
    ("^", "<"): ["v<"],
    ("^", "v"): ["v"],
    ("^", ">"): ["v>", ">v"],
    # A
    ("A", "A"): [""],
    ("A", "^"): ["<"],
    ("A", "<"): ["<v<", "v<<"],
    ("A", "v"): ["<v", "v<"],
    ("A", ">"): ["v"],
    # <
    ("<", "<"): [""],
    ("<", "^"): [">^"],
    ("<", "A"): [">^>", ">>^"],
    ("<", "v"): [">"],
    ("<", ">"): [">>"],
    # v
    ("v", "v"): [""],
    ("v", "^"): ["^"],
    ("v", "A"): ["^>", ">^"],
    ("v", "<"): ["<"],
    ("v", ">"): [">"],
    # >
    (">", ">"): [""],
    (">", "^"): ["^<", "<^"],
    (">", "A"): ["^"],
    (">", "<"): ["<<"],
    (">", "v"): ["<"],
}

NUM_PAD = {}

NUM_MOVES = {
    "A": [("0", "<"), ("3", "^")],
    "0": [("A", ">"), ("2", "^")],
    "1": [("4", "^"), ("2", ">")],
    "2": [("1", "<"), ("5", "^"), ("3", ">"), ("0", "v")],
    "3": [("6", "^"), ("2", "<"), ("A", "v")],
    "4": [("7", "^"), ("5", ">"), ("1", "v")],
    "5": [("4", "<"), ("8", "^"), ("6", ">"), ("2", "v")],
    "6": [("9", "^"), ("5", "<"), ("3", "v")],
    "7": [("8", ">"), ("4", "v")],
    "8": [("7", "<"), ("9", ">"), ("5", "v")],
    "9": [("8", "<"), ("6", "v")],
}


def part1(codes):
    initialize_num_pad()

    factors = [int(code[:-1]) for code in codes]
    lengths = [key_length(code) for code in codes]
    complexities = [l * a for l, a in zip(lengths, factors)]

    return sum(complexities)


def key_length(code):
    numpad_seq = numpad_sequences(code)

    dpad1_seq = [seq for numpad in numpad_seq for seq in dpad_sequences(numpad)]

    min_len = min(len(seq) for seq in dpad1_seq)
    dpad1_seq = [seq for seq in dpad1_seq if len(seq) == min_len]

    dpad2_seq = [seq for dpad1 in dpad1_seq for seq in dpad_sequences(dpad1)]

    return min(len(seq) for seq in dpad2_seq)


def numpad_sequences(code):
    cur = "A"
    seqs = [""]
    for key in code:
        seqs = [f"{s}{m}A" for s in seqs for m in NUM_PAD[(cur, key)]]
        cur = key
    return seqs


def dpad_sequences(moves):
    cur = "A"
    seqs = [""]
    for key in moves:
        seqs = [f"{s}{m}A" for s in seqs for m in DIRECTIONAL[(cur, key)]]
        cur = key
    return seqs


def initialize_num_pad():
    for src in "A0123456789":
        for tgt in "A0123456789":
            if src == tgt:
                NUM_PAD[(src, tgt)] = [""]
            else:
                NUM_PAD[(src, tgt)] = shortest_num_pad_paths(src, tgt)


def shortest_num_pad_paths(src, tgt):
    dist = {}
    prev = {}
    queue = []

    for node in NUM_MOVES:
        dist[node] = float("inf")
        prev[node] = None
        queue.append(node)

    dist[src] = 0

    while queue:
        u = min(queue, key=lambda node: dist[node])
        queue.remove(u)

        for v, dir in NUM_MOVES[u]:
            alt = dist[u] + 1
            if alt < dist[v]:
                dist[v] = alt
                prev[v] = [(u, dir)]
            elif alt == dist[v]:
                prev[v].append((u, dir))

    all_shortest_paths = []

    def reconstruct_paths(v, path):
        if v == src:
            all_shortest_paths.append(path)
        else:
            for u, dir in prev[v]:
                reconstruct_paths(u, dir + path)

    reconstruct_paths(tgt, "")

    return all_shortest_paths


class Tests(unittest.TestCase):
    pass


if __name__ == "__main__":
    flags = set(arg for arg in sys.argv[1:] if arg.startswith("-"))
    args = [arg for arg in sys.argv[1:] if not arg.startswith("-")]
    sys.argv = sys.argv[:1]  # strip args, unittest.main() doesn't like them

    filename = "sample.txt" if "-s" in flags or "--sample" in flags else "input.txt"
    filename = args[0] if args else filename
    is_sample = filename.startswith("sample")
    run_tests = "-t" in flags or "--test" in flags

    if run_tests:
        unittest.main(exit=True)

    def check(part, actual, expected=None):
        print(f"Part {part}{' (sample)' if is_sample else ''}: {actual} ", end="")
        if expected is None:
            print("❔")
        else:
            if actual != expected:
                print(f"≠ {expected} ❌")
                exit(1)
            print("✅")

    codes = [line.strip() for line in open(filename).readlines()]
    p1 = part1(codes)
    p2 = None

    check(1, p1, 126384 if is_sample else 163086)
    check(2, p2)
