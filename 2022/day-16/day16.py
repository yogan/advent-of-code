import unittest, sys
from collections import defaultdict

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
file = sys.argv[1]
sys.argv = sys.argv[:1] # strip args, they scare the unittest module
is_sample = file != "input.txt"

def parse():
    with open(file) as f:
        lines = []
        for line in f.read().splitlines():
            words = line.split(";")
            lines.append(words)
        return lines


def part1():
    lines = parse()
    return "TODO"

class TestDay16(unittest.TestCase):
    def test_parse(self):
        lines = parse()
        if is_sample:
            self.assertEqual(10, len(lines))
        else:
            self.assertEqual(60, len(lines))

if __name__ == '__main__':
    unittest.main(exit=False)
    print()


    res1 = part1()
    print(f"Part 1: {res1}", "(sample)" if is_sample else "")

    # res2 = part2()
    # print(f"Part 2: {res2}", "(sample)" if is_sample else "")
