import sys
import unittest
from collections import deque


def parse():
    vs, gs = open(filename).read().split("\n\n")
    values = dict()
    for line in vs.splitlines():
        wire, value = line.split(": ")
        values[wire] = int(value)
    gates = deque()
    for line in gs.splitlines():
        parts = line.split()
        gates.append({"a": parts[0], "b": parts[2], "op": parts[1], "out": parts[-1]})
    return values, gates


def simulate(values, gates):
    while gates:
        gate = gates.popleft()
        a, b, op, out = gate["a"], gate["b"], gate["op"], gate["out"]

        if a not in values or b not in values:
            gates.append(gate)
            continue

        if op == "AND":
            values[out] = values[a] & values[b]
        elif op == "OR":
            values[out] = values[a] | values[b]
        elif op == "XOR":
            values[out] = values[a] ^ values[b]

    zs = [(k, v) for k, v in values.items() if k.startswith("z")]
    zs.sort(key=lambda x: x[0], reverse=True)

    bitstring = ""
    for _, v in zs:
        bitstring += "1" if v % 2 else "0"

    return int(bitstring, 2)


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

    values, wires = parse()
    part1 = simulate(values, wires)
    part2 = None

    check(1, part1, 2024 if is_sample else 65740327379952)
    check(2, part2)
