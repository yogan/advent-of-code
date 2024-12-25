import sys
from collections import deque
from itertools import combinations
from random import randint

from tqdm import tqdm


def parse():
    vs, gs = open(filename).read().split("\n\n")
    values = dict()
    for line in vs.splitlines():
        wire, value = line.split(": ")
        values[wire] = int(value)
    gates = []
    for line in gs.splitlines():
        a, op, b, _, out = line.split()
        gate = {"a": a, "b": b, "op": op, "out": out}
        gates.append(gate)
    return values, gates


def simulate(values, gates):
    steps = 0
    queue = deque(gates)

    while queue:
        # poor man's cycle detection
        steps += 1
        if steps > 7500:
            return -1

        gate = queue.popleft()
        a, b, op, out = gate["a"], gate["b"], gate["op"], gate["out"]

        if a not in values or b not in values:
            queue.append(gate)
            continue

        if op == "AND":
            values[out] = values[a] & values[b]
        elif op == "OR":
            values[out] = values[a] | values[b]
        elif op == "XOR":
            values[out] = values[a] ^ values[b]

    return read("z", values)


def read(var, values):
    result = 0
    for value in values:
        if not value.startswith(var):
            continue
        result |= values[value] << int(value[1:])
    return result


def create_values(x, y):
    values = dict()
    for i in range(45):
        values[f"x{i:02}"] = (x >> i) & 1
        values[f"y{i:02}"] = (y >> i) & 1
    return values


def test_bit(bit, gates):
    test_values = [
        (x << bit, y << bit, z << bit)
        for x, y, z in (
            (0, 0, 0),
            (1, 0, 1),
            (0, 1, 1),
        )
    ]

    for x, y, r in test_values:
        if simulate(create_values(x, y), gates) != r:
            return False

    return True


def test_1000_random_values(gates):
    for _ in range(1000):
        a, b = randint(0, 2**44 - 1), randint(0, 2**44 - 1)
        if simulate(create_values(a, b), gates) != a + b:
            return False
    return True


def swap_outs(a, b, gates):
    for gate in gates:
        if gate["out"] == a:
            gate["out"] = b
        elif gate["out"] == b:
            gate["out"] = a


def find_wire_swaps(gates):
    wires = [g["out"] for g in gates]
    swap_combinations = [(w1, w2) for w1 in wires for w2 in wires if w1 < w2]
    candidates = set()

    for bit in tqdm(range(45), desc=f"Testing bits"):
        if test_bit(bit, gates):
            continue

        for o1, o2 in tqdm(swap_combinations, desc=f"Fixing bit {bit}", leave=False):
            swap_outs(o1, o2, gates)
            if test_bit(bit, gates):
                candidates.add((o1, o2))
            swap_outs(o1, o2, gates)

    for swaps in combinations(candidates, 4):
        wires = set(w for s in swaps for w in s)

        if len(wires) != 8:
            continue

        for o1, o2 in swaps:
            swap_outs(o1, o2, gates)

        if test_1000_random_values(gates):
            return ",".join(sorted(wires))

        for o1, o2 in swaps:
            swap_outs(o1, o2, gates)


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

    values, gates = parse()
    part1 = simulate(values, gates)
    part2 = "n/a" if is_sample else find_wire_swaps(gates)

    check(1, part1, 2024 if is_sample else 65740327379952)
    check(2, part2, "n/a" if is_sample else "bgs,pqc,rjm,swt,wsv,z07,z13,z31")
