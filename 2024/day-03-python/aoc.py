import re
import sys
import unittest


def scan(lines, part2=False):
    all_lines_combined = "".join(lines).replace("\n", "")
    if part2:
        all_lines_combined = strip_dont_ranges(all_lines_combined)
    return eval_muls(all_lines_combined)


def strip_dont_ranges(line):
    stripped_line = ""

    while "don't()" in line:
        parts = line.split("don't()", maxsplit=1)
        stripped_line += parts[0]
        parts = parts[1].split("do()", maxsplit=1)
        if len(parts) > 1:
            line = parts[1]
        else:
            line = ""

    return stripped_line + line


def eval_muls(line):
    return sum(
        int(x) * int(y) for x, y in re.findall(r"mul\((\d{1,3}),(\d{1,3})\)", line)
    )


class Tests(unittest.TestCase):
    def test_strip_dont_ranges(self):
        self.assertEqual(
            strip_dont_ranges(
                "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
            ),
            "xmul(2,4)&mul[3,7]!^?mul(8,5))",
        )

    def test_eval_muls(self):
        self.assertEqual(
            eval_muls(
                "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
            ),
            2 * 4 + 5 * 5 + 11 * 8 + 8 * 5,
        )


if __name__ == "__main__":
    flags = set(arg for arg in sys.argv[1:] if arg.startswith("-"))
    args = [arg for arg in sys.argv[1:] if not arg.startswith("-")]

    filename = "sample.txt" if "-s" in flags or "--sample" in flags else "input.txt"
    filename = args[0] if args else filename
    is_sample = filename.startswith("sample")
    run_tests = "-t" in flags or "--test" in flags

    if run_tests:
        unittest.main(argv=sys.argv[:1])

    def check(part, actual, expected=None):
        print(f"Part {part}{' (sample)' if is_sample else ''}: {actual} ", end="")
        if expected is None:
            print("❔")
        else:
            if actual != expected:
                print(f"≠ {expected} ❌")
                exit(1)
            print("✅")

    lines = open(filename).readlines()
    part1 = scan(lines)
    part2 = scan(lines, part2=True)

    check(1, part1, 161 + 161 if is_sample else 187194524)
    check(2, part2, 161 + 48 if is_sample else 127092535)
