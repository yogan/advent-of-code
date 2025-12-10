import sys
import unittest
from collections import deque


def part1(machines):
    return sum(fewest_presses(target, buttons) for target, buttons, _ in machines)


def fewest_presses(target, buttons):
    len_buttons = len(buttons)
    initial_lights = tuple([False] * len(target))
    queue = deque([(initial_lights, [0] * len_buttons)])
    seen = set([initial_lights])

    while True:
        lights, presses = queue.popleft()

        if lights == target:
            return sum(presses)

        for i in range(len_buttons):
            new_lights = press(lights, buttons[i])
            if new_lights in seen:
                continue
            seen.add(new_lights)
            new_presses = [p + 1 if i == j else p for j, p in enumerate(presses)]
            queue.append((new_lights, new_presses))


def press(lights, button):
    return tuple(not light if i in button else light for i, light in enumerate(lights))


def parse():
    return [parse_machine(line.strip()) for line in open(filename).readlines()]


def parse_machine(line):
    target, *buttons, joltages = [part[1:-1] for part in line.split()]

    target = tuple(ch == "#" for ch in target)
    buttons = [list(map(int, b.split(","))) for b in buttons]
    joltages = [int(j) for j in joltages.split(",")]

    return target, buttons, joltages


class Tests(unittest.TestCase):
    def test_parse_machine(self):
        target, buttons, joltages = parse_machine(
            "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
        )
        self.assertEqual(target, (False, True, True, False))
        self.assertEqual(buttons, [[3], [1, 3], [2], [2, 3], [0, 2], [0, 1]])
        self.assertEqual(joltages, [3, 5, 4, 7])

    def test_press(self):
        lights = (False, True, True, False)
        self.assertEqual(press(lights, [3]), (False, True, True, True))
        self.assertEqual(press(lights, [1, 3]), (False, False, True, True))
        self.assertEqual(press(lights, [2]), (False, True, False, False))
        self.assertEqual(press(lights, [2, 3]), (False, True, False, True))
        self.assertEqual(press(lights, [0, 2]), (True, True, False, False))
        self.assertEqual(press(lights, [0, 1]), (True, False, True, False))


def main():
    failures = 0
    failures += check(1, part1(parse()), 7 if is_sample else 449)

    exit(failures)


def check(part, actual, expected=None):
    failure = 0

    if expected is None:
        symbol = "🤔"
        result = f"{actual}"
    elif actual == expected:
        symbol = "✅"
        result = f"{actual}"
    else:
        symbol = "❌"
        result = f"{actual} ≠ {expected}"
        failure = 1

    print(f"{symbol} Part {part}{' (sample)' if is_sample else ''}: {result}")
    return failure


if __name__ == "__main__":
    flags = set(arg for arg in sys.argv[1:] if arg.startswith("-"))
    args = [arg for arg in sys.argv[1:] if not arg.startswith("-")]

    is_sample = "-s" in flags or "--sample" in flags
    run_tests = "-t" in flags or "--test" in flags

    filename = "sample.txt" if is_sample else "input.txt"
    filename = args[0] if args else filename

    if run_tests:
        unittest.main(argv=sys.argv[:1])

    main()
