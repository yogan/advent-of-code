from input import read_and_solve


def parse_fish(line):
    return list(map(int, line.split(",")))


def part1(lines):
    days = 80
    fishes = parse_fish(lines[0])

    for _ in range(days):
        new_fishes = []
        spawned_fishes = []
        for fish in fishes:
            if fish == 0:
                new_fishes.append(6)
                spawned_fishes.append(8)
            else:
                new_fishes.append(fish-1)

        new_fishes.extend(spawned_fishes)
        fishes = new_fishes

    return len(fishes)


def part2(lines):
    days = 256
    fishes = parse_fish(lines[0])

    fish_can = {}

    for i in range(9):
        #fish_can[i] = len(list(filter(lambda f: f == i, fishes)))
        fish_can[i] = len([x for x in fishes if x == i])

    for _ in range(days):
        next_fish_can = {}

        for i in range(1, 9):
            next_fish_can[i-1] = fish_can[i]

        next_fish_can[6] += fish_can[0]
        next_fish_can[8] = fish_can[0]
        fish_can = next_fish_can

    return sum(fish_can.values())


read_and_solve(__file__, part1, part2)
