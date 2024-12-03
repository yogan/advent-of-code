import re
import sys
import unittest


def parse_lines(lines, pre_proc=False):
    all_lines_combined = "".join(lines).replace("\n", "")
    if pre_proc:
        all_lines_combined = preprocess_line(all_lines_combined)
    return parse(all_lines_combined)


def preprocess_line(line):
    processed_line = ""

    while "don't()" in line:
        parts = line.split("don't()", maxsplit=1)
        processed_line += parts[0]
        parts = parts[1].split("do()", maxsplit=1)
        if len(parts) > 1:
            line = parts[1]
        else:
            line = ""

    return processed_line + line


def parse(line):
    return [
        [int(x) for x in match]
        for match in re.findall(r"mul\((\d{1,3}),(\d{1,3})\)", line)
    ]


def add_muls(muls):
    return sum(x * y for x, y in muls)


class Tests(unittest.TestCase):
    def test_parse(self):
        line = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
        self.assertEqual(
            parse(line),
            [[2, 4], [5, 5], [11, 8], [8, 5]],
        )

    def test_preprocess_line(self):
        self.assertEqual(
            preprocess_line(
                "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
            ),
            "xmul(2,4)&mul[3,7]!^?mul(8,5))",
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

    lines = open(filename).readlines()
    muls = parse_lines(lines)
    part1 = add_muls(muls)

    muls2 = parse_lines(lines, pre_proc=True)
    part2 = add_muls(muls2)

    check(1, part1, 161 + 161 if is_sample else 187194524)
    check(2, part2, 161 + 48 if is_sample else 127092535)
