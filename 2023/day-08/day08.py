import sys, unittest
from collections import defaultdict
from enum import Enum

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
sys.argv  = sys.argv[:1] # strip args, they scare the unittest module
is_sample = filename.startswith("sample")

def parse_map():
    with open(filename) as f:
        top, bottom = f.read().split("\n\n")
    map = {}
    for line in bottom.strip().split("\n"):
        start, destinations = line.split(" = ")
        map[start] = destinations[1:-1].split(", ")
    return list(top), map

def travel(turns, map):
    steps = 0
    cur = "AAA"
    while cur != "ZZZ":
        direction = turns[steps % len(turns)]
        cur = map[cur][0] if direction == "L" else map[cur][1]
        steps += 1
    return steps

class TestDay08(unittest.TestCase):
    pass

if __name__ == '__main__':
    unittest.main(exit=False)
    print()

    turns, map = parse_map()
    steps = travel(turns, map)

    res1 = steps
    assert res1 == (6 if is_sample else 12599)
    print(f"Part 1: {res1}{' (sample)' if is_sample else ''}")

    res2 = None
    # assert res2 == (5905 if is_sample else 247885995)
    print(f"Part 2: {res2}{' (sample)' if is_sample else ''}")
