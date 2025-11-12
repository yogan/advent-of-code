import sys

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
is_sample = filename.startswith("sample")

def parse():
    with open(filename) as f:
        lines = f.read().strip().split("\n")
    return [list(map(int, line.split())) for line in lines]

def predict(history):
    lines = [history]

    line_idx = 0
    while not all(x == 0 for x in lines[line_idx]):
        next_line = []
        for i in range(len(lines[line_idx]) - 1):
            next_line.append(lines[line_idx][i + 1] - lines[line_idx][i])
        lines.append(next_line)
        line_idx += 1

    for i in range(len(lines) - 2, -1, -1):
        lines[i].append(lines[i + 1][-1] + lines[i][-1])
        lines[i].insert(0, lines[i][0] - lines[i + 1][0])

    return (lines[0][-1], lines[0][0])

if __name__ == '__main__':
    histories = parse()
    predictions = [predict(history) for history in histories]

    res1 = sum(x[0] for x in predictions)
    assert res1 == (114 if is_sample else 1725987467)
    print(f"Part 1: {res1}{' (sample)' if is_sample else ''}")

    res2 = sum(x[1] for x in predictions)
    assert res2 == (2 if is_sample else 971)
    print(f"Part 2: {res2}{' (sample)' if is_sample else ''}")
