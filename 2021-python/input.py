import os

expected = {
    # sample part 1, real part 1, sample part 2, real part 2
    "01": (7, 1564, 5, 1611),
    "02": (150, 1936494, 900, 1997106066),
    "03": (198, 4006064, 230, 5941884),
    "04": (4512, 41668, 1924, 10478),
    "05": (5, 5280, 12, 16716),
    "06": (5934, 360761, 26984457539, 1632779838045),
    "07": (37, 345197, 168, 96361606),
    "08": (26, 294, None, None),  # part 2 not solved
    "09": (15, 594, 1134, 858494),
    "10": (26397, 343863, 288957, 2924734236),
    "11": (1656, 1546, 195, 471),  # not in this repo, but solved, see README.md
    "12": (226, 4792, 3509, 133360),
    "13": (17, 682, None, None),  # part 2 is visual (solved)
    "14": (1588, 2851, 2188189693529, 10002813279337),
    "15": (40, 435, 315, 2842),
    "20": (35, 5483, None, None),  # part 1 only
    "21": (739785, 888735, 444356092776315, 647608359455719),
    "22": (590784, 600458, None, None),  # part 1 only
    "25": (58, 360, None, None),  # part 1 only
}


def assert_parts(part1, part2, day, is_sample):
    if day not in expected:
        print("WARNING: no expected values for day", day)
        return

    expected_part1 = expected[day][0 if is_sample else 1]
    expected_part2 = expected[day][2 if is_sample else 3]

    if expected_part1 is not None:
        assert part1 == expected_part1, (
            f"part 1{' (sample)' if is_sample else ''} "
            + f"should be {expected_part1}, but was {part1}"
        )

    if expected_part2 is not None:
        assert part2 == expected_part2, (
            f"part 2{' (sample)' if is_sample else ''} "
            + f"should be {expected_part2}, but was {part2}"
        )


def read_and_solve(path, solve_part_1, solve_part_2):
    filename = os.path.basename(path)
    day = filename.replace("day", "").replace(".py", "")

    input_sample = f"inputs/{day}/sample.txt"
    input_real = f"inputs/{day}/input.txt"

    def process(input, name):
        is_sample = "sample" in input
        try:
            with open(input) as file:
                lines = [line.strip() for line in file.readlines()]
                part1 = solve_part_1(lines)
                part2 = solve_part_2(lines)
                print(name)
                print("-" * len(name))
                print("Part 1:", part1)
                print("Part 2:", part2)
                assert_parts(part1, part2, day, is_sample)
        except IOError:
            print(input, "does not exist, skipping")

    print("========")
    print(f" DAY {day}")
    print("========")
    print()

    process(input_sample, "Sample")
    print()
    process(input_real, "Input")
