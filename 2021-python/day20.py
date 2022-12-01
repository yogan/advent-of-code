import unittest
from input import read_and_solve


def parse_input(lines):
    index = lines.index("")
    return lines[:index][0], list(map(list, lines[index+1:]))


def add_border(image):
    for row in image:
        row.insert(0, ".")
        row.append(".")

    image.insert(0, list("." * len(image[0])))
    image.append(list("." * len(image[0])))


def enhance(algo, image):
    add_border(image)
    add_border(image)

    enhanced = [[] for _ in range(len(image) - 2)]

    for r in range(1, len(image) - 1):
        for c in range(1, len(image[0]) - 1):
            bin_digits = []
            for dr in [-1, 0, 1]:
                for dc in [-1, 0, 1]:
                    bin_digits.append("0" if image[r+dr][c+dc] == "." else "1")
            offset = int("".join(bin_digits), 2)
            enhanced[r-1].append(algo[offset])

    return enhanced

def count_pixels(image):
    pixels = [pixel for row in image for pixel in row]
    return pixels.count("#")


def part1(lines):
    algo, image = parse_input(lines)
    for _ in range(2):
        image = enhance(algo, image)
    return count_pixels(image)


def part2(lines):
    algo, image = parse_input(lines)
    for _ in range(50):
        image = enhance(algo, image)
    return count_pixels(image)


class TestDay20(unittest.TestCase):

    sample = [
        "".join([
            "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##",
            "#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###",
            ".######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.",
            ".#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....",
            ".#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..",
            "...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....",
            "..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#",
        ]),
        "",
        "#..#.",
        "#....",
        "##..#",
        "..#..",
        "..###",
    ]

    def test_parse_input(self):
        algo, image = parse_input(self.sample)

        self.assertEqual(len(algo), 512)
        self.assertEqual(image, [
            ["#", ".", ".", "#", "."],
            ["#", ".", ".", ".", "."],
            ["#", "#", ".", ".", "#"],
            [".", ".", "#", ".", "."],
            [".", ".", "#", "#", "#"],
        ])

    def test_add_border(self):
        image = [
            list("#..#."),
            list("#...."),
            list("##..#"),
            list("..#.."),
            list("..###"),
        ]

        add_border(image)

        self.assertEqual(image, [
            list("......."),
            list(".#..#.."),
            list(".#....."),
            list(".##..#."),
            list("...#..."),
            list("...###."),
            list("......."),
        ])

        add_border(image)

        self.assertEqual(image, [
            list("........."),
            list("........."),
            list("..#..#..."),
            list("..#......"),
            list("..##..#.."),
            list("....#...."),
            list("....###.."),
            list("........."),
            list("........."),
        ])

    def test_enhance(self):
        algo = self.sample[0]
        image = [
            list("#..#."),
            list("#...."),
            list("##..#"),
            list("..#.."),
            list("..###"),
        ]

        enhanced_image = enhance(algo, image)

        self.assertEqual(enhanced_image, [
            list(".##.##."),
            list("#..#.#."),
            list("##.#..#"),
            list("####..#"),
            list(".#..##."),
            list("..##..#"),
            list("...#.#."),
        ])

    def test_count_pixels(self):
        image = [
            list("#..#."),
            list("#...."),
            list("##..#"),
            list("..#.."),
            list("..###"),
        ]

        pixels = count_pixels(image)

        self.assertEqual(pixels, 10)

    def test_part_1_sample(self):
        self.assertEqual(part1(self.sample), 35)

    def test_part_2_sample(self):
        self.assertEqual(part2(self.sample), 3351)


if __name__ == '__main__':
    unittest.main(exit=False)
    read_and_solve(__file__, part1, part2)
