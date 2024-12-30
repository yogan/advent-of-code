import sys
import unittest
from functools import cache

# This movement table will always give an optimal movement sequence for the next
# deeper dpad to make a single move on the current dpad.
# When there were multiple possible ways, those heuristics are applied:
# - Multiple same keys in a row are ideal, as those will only be additional A
#   presses on the next dpad: v<< or <<v are better than <v<.
# - Going to < in the bottom right is costly, as it is the farthest away from A,
#   to which we have to return every time.
# - More general, going away from A is costly, and we should press buttons in
#   order from worst to best. This already gives this order: <  v  (^ or >)  A
# - The order between ^ and >: ^ first is better. This was found out by trial
#   and error; I don't have a good explanation for this.
DIRECTIONAL = {
    # ^
    ("^", "^"): "",
    ("^", "A"): ">",
    ("^", "<"): "v<",
    ("^", "v"): "v",
    ("^", ">"): "v>",  # better than >v
    # A
    ("A", "A"): "",
    ("A", "^"): "<",
    ("A", "<"): "v<<",  # better than "<v<"
    ("A", "v"): "<v",
    ("A", ">"): "v",
    # <
    ("<", "<"): "",
    ("<", "^"): ">^",
    ("<", "A"): ">>^",  # better than ">^>"
    ("<", "v"): ">",
    ("<", ">"): ">>",
    # v
    ("v", "v"): "",
    ("v", "^"): "^",
    ("v", "A"): "^>",  # better than >^
    ("v", "<"): "<",
    ("v", ">"): ">",
    # >
    (">", ">"): "",
    (">", "^"): "<^",  # better than ^<
    (">", "A"): "^",
    (">", "<"): "<<",
    (">", "v"): "<",
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


def calc(codes, levels):
    initialize_num_pad()

    factors = [int(code[:-1]) for code in codes]
    lengths = [key_length(code, levels) for code in codes]
    complexities = [l * a for l, a in zip(lengths, factors)]

    return sum(complexities)


def key_length(code, levels):
    return min(encode(ns, levels) for ns in numpad_sequences(code))


@cache
def encode(seq, level):
    if seq == "":
        return 0

    if level == 0:
        return len(seq)

    front, rest = seq.split("A", 1)
    return encode(dpad_sequence(front + "A"), level - 1) + encode(rest, level)


def numpad_sequences(code):
    cur = "A"
    seqs = [""]
    for key in code:
        seqs = [f"{s}{m}A" for s in seqs for m in NUM_PAD[(cur, key)]]
        cur = key
    return seqs


def dpad_sequence(moves):
    cur = "A"
    seq = ""
    for key in moves:
        seq += DIRECTIONAL[(cur, key)] + "A"
        cur = key
    return seq


def initialize_num_pad():
    for src in "A0123456789":
        for tgt in "A0123456789":
            if src == tgt:
                NUM_PAD[(src, tgt)] = [""]
            else:
                NUM_PAD[(src, tgt)] = shortest_num_pad_paths(src, tgt)

    for np in NUM_PAD:
        NUM_PAD[np] = prune_moves(NUM_PAD[np])


def prune_moves(moves):
    rated_moves = [(m, changes(m)) for m in moves]
    min_changes = min(changes for _, changes in rated_moves)
    return [move for move, changes in rated_moves if changes == min_changes]


def changes(move):
    return sum(1 for i in range(len(move) - 1) if move[i] != move[i + 1])


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
    def test_encode(self):
        numpad_seq = "<A^A>^^AvvvA"
        self.assertEqual(encode(numpad_seq, 0), len(numpad_seq))
        self.assertEqual(encode(numpad_seq, 1), len("v<<A>>^A<A>AvA<^AA>A<vAAA>^A"))
        self.assertEqual(
            encode(numpad_seq, 2),
            len("<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A"),
        )


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
    check(1, calc(codes, 2), 126384 if is_sample else 163086)
    check(2, calc(codes, 25), 154115708116294 if is_sample else 198466286401228)
