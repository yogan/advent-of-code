import sys
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

def gcd(a, b):
    while b:
        a, b = b, a % b
    return a

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

    ends_seen = {}
    for idx, waypoints in seen.items():
        ends_seen[idx] = [(i, pos) for i, pos in enumerate(waypoints) if pos[0].endswith("Z")]

    indices = [x[0][0] + 1 for x in ends_seen.values()]
    return lcm(indices)

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
