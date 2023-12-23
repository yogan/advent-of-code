import sys, math

if len(sys.argv) != 2:
    print("Missing input file.")
    exit(1)
filename  = sys.argv[1]
is_sample = filename == "sample.txt"

def parse(remove_slopes):
    if remove_slopes:
        return [line.strip().translate(str.maketrans(">v<^", "...."))
                for line in open(filename).readlines()]

    return [line.strip() for line in open(filename).readlines()]

def crossings(trails):
    nodes: set[tuple[int, int]] = set()

    for r, row in enumerate(trails):
        for c, ch in enumerate(row):
            if ch == '#':
                continue
            n = len(neighbors((r, c), trails, consider_slopes=False))
            if n == 1 or n > 2:  # 1 = start/end, >2 = crossing
                nodes.add((r, c))

    (start,) = (n for n in nodes if n[0] == 0)
    (end,)   = (n for n in nodes if n[0] == len(trails)-1)

    return nodes, start, end

def neighbors(pos, trails, consider_slopes=True):
    R, C = len(trails), len(trails[0])
    r, c = pos
    ch = trails[r][c]

    if consider_slopes and ch == '>':
        candidates = [(r, c+1)]
    elif consider_slopes and ch == 'v':
        candidates = [(r+1, c)]
    elif consider_slopes and ch == '<':
        candidates = [(r, c-1)]
    elif consider_slopes and ch == '^':
        candidates = [(r-1, c)]
    else:
        candidates = [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]

    return [(r, c) for r, c in candidates
            if 0 <= r < R and 0 <= c < C and trails[r][c] != '#']

def build_graph(trails):
    graph = {}
    nodes, start, end = crossings(trails)

    for sr, sc in nodes:
        graph[(sr, sc)] = {}
        stack: list[tuple[int, int, int]] = [(0, sr, sc)]
        seen = {(sr, sc)}

        while stack:
            dist, r, c = stack.pop()

            if dist != 0 and (r, c) in nodes:
                graph[(sr, sc)][(r, c)] = dist
                continue

            for nr, nc in neighbors((r, c), trails):
                if (nr, nc) in seen:
                    continue
                seen.add((nr, nc))
                stack.append((dist + 1, nr, nc))

    return graph, start, end

def dfs(graph, start, end, seen=set()):
    if start == end:
        return 0

    longest = -math.inf

    seen.add(start)

    for target, distance in graph[start].items():
        if target in seen:
            continue
        longest = max(longest, distance + dfs(graph, target, end, seen))

    seen.remove(start)

    return longest

def longest_path(remove_slopes):
    trails = parse(remove_slopes)
    graph, start, end = build_graph(trails)

    return dfs(graph, start, end)

def check(part, actual, expected=None):
    print(f"Part {part}{' (sample)' if is_sample else ''}: {actual} ", end="")
    if expected is None:
        print("❔")
    else:
        if actual != expected:
            print(f"≠ {expected} ❌")
            exit(1)
        print("✅")

if __name__ == '__main__':
    part1 = longest_path(remove_slopes=False)
    part2 = longest_path(remove_slopes=True)

    check(1, part1,  94 if is_sample else 2114)
    check(2, part2, 154 if is_sample else 6322)
