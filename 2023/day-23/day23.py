import sys
from collections import deque

visualize = "--visualize" in sys.argv
if visualize:
    sys.argv.remove("--visualize")
if len(sys.argv) != 2:
    print("Missing input file.")
    exit(1)
filename  = sys.argv[1]
is_sample = filename == "sample.txt"

def parse():
    return [line.strip() for line in open(filename).readlines()]

def hike(trails):
    R, C = len(trails), len(trails[0])
    r, c = 0, 1
    end_r, end_c = R-1, C-2
    assert trails[r][c] == '.' and trails[end_r][end_c] == '.'

    visited = set()
    all_paths = []
    queue = deque()
    queue.append((r, c, []))

    while queue:
        r, c, path_so_far = queue.pop()

        if (r, c) == (end_r, end_c):
            all_paths.append(path_so_far + [(r, c)])

            if visualize and is_sample:
                print(f"Path #{len(all_paths)}:")
                branch_starts = [(r,c) for r,c,_ in queue]
                draw(trails, all_paths[-1], branch_starts)

            if queue:
                _, _, path_so_far = queue[-1]
                visited = set(path_so_far)
                continue

        if (r, c) in visited:
            continue
        visited.add((r, c))

        assert trails[r][c] not in ['<', '^']  # neither in sample, nor in input

        for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
            nr, nc = r + dr, c + dc
            if nr < 0 or nr >= R or nc < 0 or nc >= C \
                    or trails[nr][nc] == '#' or (nr, nc) in visited:
                continue

            if trails[nr][nc] == '>':
                nnr, nnc = nr, nc + 1
                if nnc < C and trails[nnr][nnc] != '#' and (nnr, nnc) not in visited:
                    visited.add((r, c))
                    queue.append((nnr, nnc, path_so_far + [(r,c), (nr,nc)]))
                continue

            elif trails[nr][nc] == 'v':
                nnr, nnc = nr + 1, nc
                if nnr < R and trails[nnr][nnc] != '#' and (nnr, nnc) not in visited:
                    visited.add((r, c))
                    queue.append((nnr, nnc, path_so_far + [(r,c), (nr,nc)]))
                continue

            else:
                queue.append((nr, nc, path_so_far + [(r,c)]))

    return all_paths

def draw_char(r, c, ch, path, branch_starts):
    reset  = f"\033{chr(91)}0m"
    green  = f"\033{chr(91)}32m"
    yellow = f"\033{chr(91)}33m"
    blue   = f"\033{chr(91)}34m"

    if path and (r, c) == path[0]:
        print(f"{green}🠗{reset}", end="")
    elif path and (r, c) == path[-1]:
        print(f"{green}⭳{reset}", end="")
    elif (r, c) in path:
        print(f"{blue}🞄{reset}", end="")
    else:
        if (r, c) in branch_starts:
            print(f"{yellow}×{reset}", end="")
        else:
            print(ch.translate(ch.maketrans(".#>v", " ▒▶▼")), end="")

def draw(trails, path=[(0, 1)], branch_starts=[]):
    for r, row in enumerate(trails):
        for c, ch in enumerate(row):
            draw_char(r, c, ch, path, branch_starts)
        print()
    print()

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
    trails = parse()

    if visualize and is_sample:
        print("Hike map:")
        draw(trails)

    paths = hike(trails)
    steps = [len(path) - 1 for path in paths]
    longest = max(steps)

    if visualize:
        print("Longest path:")
        longest_path = paths[steps.index(longest)]
        draw(trails, longest_path)

    part1 = longest
    part2 = None

    check(1, part1, 94 if is_sample else 2114)
    check(2, part2)
