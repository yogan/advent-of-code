from input import read_and_solve


def part1(lines):
    prev = int(lines[0])
    increases = 0

    for line in lines[1::]:
        number = int(line)
        if number > prev:
            increases += 1
        prev = number

    return increases


def part2(lines):
    prev = int(lines[0]) + int(lines[1]) + int(lines[2])
    increases = 0

    for i in range(len(lines)-2):
        number = int(lines[i]) + int(lines[i+1]) + int(lines[i+2])
        if number > prev:
            increases += 1
        prev = number

    return increases


read_and_solve(__file__, part1, part2)
