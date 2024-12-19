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


def min_cost(machine):
    ax, ay = machine["a"]
    bx, by = machine["b"]
    px, py = machine["prize"]

    best = None
    for i in range(101):
        for j in range(101):
            x = ax * i + bx * j
            y = ay * i + by * j
            if (x, y) == (px, py):
                cost = 3 * i + j
                if not best or cost < best:
                    best = cost
    return best


def part1(machines):
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
    p1 = part1(machines)
    p2 = None

    check(1, p1, 480 if is_sample else 40069)
    check(2, p2)
