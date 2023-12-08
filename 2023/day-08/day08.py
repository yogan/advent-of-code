import sys
from math import gcd
from collections import defaultdict

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

def lcm(nums):
    lcm = nums[0]
    for i in nums[1:]:
        lcm = lcm * i // gcd(lcm, i)
    return lcm

def ghost_travel(turns, map):
    positions = [x for x in map.keys() if x.endswith("A")]
    seen = defaultdict(list)

    for i, pos in enumerate(positions):
        steps = 0
        while True:
            idx = steps % len(turns)
            direction = turns[idx]
            new_pos = map[pos][0] if direction == "L" else map[pos][1]
            if (new_pos, idx) in seen[i]:
                break
            seen[i].append((new_pos, idx))
            pos = new_pos
            steps += 1

    cycle_lengths = []
    for idx, waypoints in seen.items():
        ends = [pos for pos in waypoints if pos[0].endswith("Z")]
        end_indices = [i for i, pos in enumerate(waypoints) if pos in ends]
        # This solution probably only works because for the input data, there
        # is only a single cycle ending in an end node. Even for the small part
        # 2 sample, this does not hold true (seconds path reaches 22Z with a
        # left and a right direction).
        if not is_sample:
            assert len(end_indices) == 1
        cycle_lengths.append(end_indices[0] + 1)

    return lcm(cycle_lengths)

if __name__ == '__main__':
    turns, map = parse_map()

    if is_sample:
        print("Part 1: skipped for sample (different sample inputs)")
    else:
        res1 = travel(turns, map)
        assert res1 == 12599
        print(f"Part 1: {res1}")

    res2 = ghost_travel(turns, map)
    assert res2 == (6 if is_sample else 8245452805243)
    print(f"Part 2: {res2}{' (sample)' if is_sample else ''}")
