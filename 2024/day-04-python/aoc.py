import sys
import unittest


def parse():
    return [line.strip() for line in open(filename).readlines()]


def count_xmas(letters):
    n = 0

    for i in range(len(letters)):
        for j in range(len(letters[i])):
            if letters[i][j] != "X":
                continue

            # check north
            if (
                i >= 3
                and letters[i - 1][j] == "M"
                and letters[i - 2][j] == "A"
                and letters[i - 3][j] == "S"
            ):
                n += 1

            # check north-east
            if (
                i >= 3
                and j <= len(letters[i]) - 4
                and letters[i - 1][j + 1] == "M"
                and letters[i - 2][j + 2] == "A"
                and letters[i - 3][j + 3] == "S"
            ):
                n += 1

            # check east
            if (
                j <= len(letters[i]) - 4
                and letters[i][j + 1] == "M"
                and letters[i][j + 2] == "A"
                and letters[i][j + 3] == "S"
            ):
                n += 1

            # check south-east
            if (
                i <= len(letters) - 4
                and j <= len(letters[i]) - 4
                and letters[i + 1][j + 1] == "M"
                and letters[i + 2][j + 2] == "A"
                and letters[i + 3][j + 3] == "S"
            ):
                n += 1

            # check south
            if (
                i <= len(letters) - 4
                and letters[i + 1][j] == "M"
                and letters[i + 2][j] == "A"
                and letters[i + 3][j] == "S"
            ):
                n += 1

            # check south-west
            if (
                i <= len(letters) - 4
                and j >= 3
                and letters[i + 1][j - 1] == "M"
                and letters[i + 2][j - 2] == "A"
                and letters[i + 3][j - 3] == "S"
            ):
                n += 1

            # check west
            if (
                j >= 3
                and letters[i][j - 1] == "M"
                and letters[i][j - 2] == "A"
                and letters[i][j - 3] == "S"
            ):
                n += 1

            # check north-west
            if (
                i >= 3
                and j >= 3
                and letters[i - 1][j - 1] == "M"
                and letters[i - 2][j - 2] == "A"
                and letters[i - 3][j - 3] == "S"
            ):
                n += 1

    return n


def count_x_mas(letters):
    # // Checking corners clockwise like this:
    # // 0 . 1      M . M      M . S      S . M      S . S
    # // . A .  ->  . A .  or  . A .  or  . A .  or  . A .
    # // 3 . 2      S . S      M . S      S . M      M . M
    valid_corners = ["MMSS", "MSSM", "SMMS", "SSMM"]

    n = 0

    for i in range(1, len(letters) - 1):
        for j in range(1, len(letters[i]) - 1):
            if letters[i][j] != "A":
                continue

            corners = (
                letters[i - 1][j - 1]
                + letters[i - 1][j + 1]
                + letters[i + 1][j + 1]
                + letters[i + 1][j - 1]
            )

            if corners in valid_corners:
                n += 1
                continue

    return n


class Tests(unittest.TestCase):
    sample = [
        list("MMMSXXMASM"),
        list("MSAMXMSMSA"),
        list("AMXSXMAAMM"),
        list("MSAMASMSMX"),
        list("XMASAMXAMM"),
        list("XXAMMXXAMA"),
        list("SMSMSASXSS"),
        list("SAXAMASAAA"),
        list("MAMMMXMMMM"),
        list("MXMXAXMASX"),
    ]

    def test_count_xmas(self):
        self.assertEqual(count_xmas(self.sample), 18)

    def test_count_x_mas(self):
        self.assertEqual(count_x_mas(self.sample), 9)


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

    lines = parse()
    part1 = count_xmas(lines)
    part2 = count_x_mas(lines)

    check(1, part1, 18 if is_sample else 2583)
    check(2, part2, 9 if is_sample else 1978)
