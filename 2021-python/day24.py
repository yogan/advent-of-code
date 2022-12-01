from collections import deque
import unittest
from input import read_and_solve


def alu(instructions, digits):
    assert all(0 < x < 10 for x in digits)

    w, x, y, z = 0, 0, 0, 0

    for inst in instructions:
        cmd, params = inst.split(" ", 1)

        if cmd == "inp":
            if params == "w":
                w = digits.popleft()
            elif params == "x":
                x = digits.popleft()
            elif params == "y":
                y = digits.popleft()
            elif params == "z":
                z = digits.popleft()

        else:
            a, b = params.split(" ")

            if cmd == "add":
                right = 0
                if b == "w":
                    right = w
                elif b == "x":
                    right = x
                elif b == "y":
                    right = y
                elif b == "z":
                    right = z
                else:
                    right = int(b)

                if a == "w":
                    w += right
                elif a == "x":
                    x += right
                elif a == "y":
                    y += right
                elif a == "z":
                    z += right

            elif cmd == "mul":
                right = 0
                if b == "w":
                    right = w
                elif b == "x":
                    right = x
                elif b == "y":
                    right = y
                elif b == "z":
                    right = z
                else:
                    right = int(b)

                if a == "w":
                    w *= right
                elif a == "x":
                    x *= right
                elif a == "y":
                    y *= right
                elif a == "z":
                    z *= right

            elif cmd == "div":
                right = 0
                if b == "w":
                    right = w
                elif b == "x":
                    right = x
                elif b == "y":
                    right = y
                elif b == "z":
                    right = z
                else:
                    right = int(b)

                if a == "w":
                    w //= right
                elif a == "x":
                    x //= right
                elif a == "y":
                    y //= right
                elif a == "z":
                    z //= right

            elif cmd == "mod":
                right = 0
                if b == "w":
                    right = w
                elif b == "x":
                    right = x
                elif b == "y":
                    right = y
                elif b == "z":
                    right = z
                else:
                    right = int(b)

                if a == "w":
                    w %= right
                elif a == "x":
                    x %= right
                elif a == "y":
                    y %= right
                elif a == "z":
                    z %= right

            elif cmd == "eql":
                right = 0
                if b == "w":
                    right = w
                elif b == "x":
                    right = x
                elif b == "y":
                    right = y
                elif b == "z":
                    right = z
                else:
                    right = int(b)

                if a == "w":
                    w = 1 if w == right else 0
                elif a == "x":
                    w = 1 if x == right else 0
                elif a == "y":
                    w = 1 if y == right else 0
                elif a == "z":
                    w = 1 if z == right else 0

    return w, x, y, z


def part1(lines):
    # Surprise! This is not nearly fast enough to brute force it!
    # How can that be, it's just 22.876.792.454.961 inputs?!
    model_number = 99999999999999
    while model_number:
        if model_number % 1000 == 0:
            print(model_number)

        model_str = str(model_number)
        if len(model_str) < 14:
            break

        if not "0" in model_str:
            digits = deque([int(x) for x in list(model_str)])
            _, _, _, z = alu(lines, digits)
            if z == 0:
                return model_number

        model_number -= 1


def part2(lines):
    return 0


class TestDay24(unittest.TestCase):

    def test_alu_negate(self):
        negate_w = [
            "inp w",
            "mul w -1",
        ]

        negate_x = [
            "inp x",
            "mul x -1",
        ]

        negate_y = [
            "inp y",
            "mul y -1",
        ]

        negate_z = [
            "inp z",
            "mul z -1",
        ]

        w, x, y, z = alu(negate_w, deque([1]))
        self.assertEqual((w, x, y, z), (-1, 0, 0, 0))

        w, x, y, z = alu(negate_x, deque([3, 2, 1]))
        self.assertEqual((w, x, y, z), (0, -3, 0, 0))

        w, x, y, z = alu(negate_y, deque([2]))
        self.assertEqual((w, x, y, z), (0, 0, -2, 0))

        w, x, y, z = alu(negate_z, deque([9, 1, 1]))
        self.assertEqual((w, x, y, z), (0, 0, 0, -9))


def split_input_file():
    filename = "inputs/24/yogan.txt"
    num = 0
    with open(filename) as file:
        out_handle = None
        for line in file.readlines():
            if "inp" in line:
                if out_handle:
                    out_handle.close()
                    out_handle = None
                num += 1
                split_file = f"{filename}.{num:02d}"
                out_handle = open(split_file, "w")
            out_handle.write(line)
        out_handle.close()
        out_handle = None


if __name__ == '__main__':
    unittest.main(exit=False)
    split_input_file()
    # read_and_solve(__file__, part1, part2)
