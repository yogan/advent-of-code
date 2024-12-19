import re
import sys
import unittest


def parse():
    machines = []
    for block in open(filename).read().split("\n\n"):
        numbers = list(map(int, re.findall(r"\d+", block)))
        machines.append(
            {
                "a": (numbers[0], numbers[1]),
                "b": (numbers[2], numbers[3]),
                "prize": (numbers[4], numbers[5]),
            }
        )
    return machines


def patch_prizes(machines):
    for machine in machines:
        px, py = machine["prize"]
        machine["prize"] = (px + 10000000000000, py + 10000000000000)


# px = ax * i + bx * j
# py = ay * i + by * j
#
# Solving for i and j:
# ax * i = px - bx * j
# i = (px - bx * j) / ax
#
# bx * j = px - ax * i
# j = (px - ax * i) / bx
#
# ay * i = py - by * j
# i = (py - by * j) / ay
#
# by * j = py - ay * i
# j = (py - ay * i) / by
#
# Equations:
# i = (px - bx * j) / ax
# j = (px - ax * i) / bx
# i = (py - by * j) / ay
# j = (py - ay * i) / by
#
# Substituting j:
# i = (px - bx * ((py - ay * i) / by)) / ax
# i = (px - bx * py / by + bx * ay * i / by) / ax
# i = (px * by - bx * py + bx * ay * i) / (ax * by)
# i * (ax * by - bx * ay) = px * by - bx * py
# i = (px * by - bx * py) / (ax * by - bx * ay)


def min_cost(machine):
    ax, ay = machine["a"]
    bx, by = machine["b"]
    px, py = machine["prize"]

    i, i_rest = divmod(px * by - bx * py, ax * by - bx * ay)
    j, j_rest = divmod(px - ax * i, bx)

    if i_rest or j_rest:
        return None

    return 3 * i + j


def required_tokens(machines):
    costs = [min_cost(machine) for machine in machines]
    return sum(cost for cost in costs if cost is not None)


class Tests(unittest.TestCase):
    def test_min_cost(self):
        self.assertEqual(
            min_cost({"a": (94, 34), "b": (22, 67), "prize": (8400, 5400)}), 280
        )
        self.assertEqual(
            min_cost({"a": (26, 66), "b": (67, 21), "prize": (12748, 12176)}), None
        )
        self.assertEqual(
            min_cost({"a": (17, 86), "b": (84, 37), "prize": (7870, 6450)}), 200
        )
        self.assertEqual(
            min_cost({"a": (69, 23), "b": (27, 71), "prize": (18641, 10279)}), None
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

    machines = parse()
    p1 = required_tokens(machines)
    patch_prizes(machines)
    p2 = required_tokens(machines)

    check(1, p1, 480 if is_sample else 40069)
    check(2, p2, 875318608908 if is_sample else 71493195288102)
