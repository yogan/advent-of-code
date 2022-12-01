from input import read_and_solve


def part1(lines):
    distance = 0
    depth = 0

    for line in lines:
        [command, num_str] = line.split(" ")
        number = int(num_str)

        if command == "forward":
            distance += number
        elif command == "up":
            depth -= number
        elif command == "down":
            depth += number

    return distance * depth


def part2(lines):
    distance = 0
    depth = 0
    aim = 0

    for line in lines:
        [command, num_str] = line.split(" ")
        number = int(num_str)

        if command == "forward":
            distance += number
            depth += aim * number
        elif command == "up":
            aim -= number
        elif command == "down":
            aim += number

    return distance * depth


read_and_solve(__file__, part1, part2)
