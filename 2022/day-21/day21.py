import unittest, sys
from tqdm import tqdm
from enum import Enum

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
sys.argv  = sys.argv[:1] # strip args, they scare the unittest module
is_sample = filename != "input.txt"

YELL = 0
ADD  = 1
SUB  = 2
MUL  = 3
DIV  = 4

def parse(filename=filename):
    with open(filename) as f:
        monkeys = {}
        for line in f.readlines():
            words = line.strip().split()
            name = words[0].replace(":", "")
            if len(words) == 2:
                num = int(words[1])
                monkeys[name] = (YELL, num)
            elif len(words) == 4:
                left = words[1]
                right = words[3]
                if words[2] == "+":
                    monkeys[name] = (ADD, left, right)
                elif words[2] == "-":
                    monkeys[name] = (SUB, left, right)
                elif words[2] == "*":
                    monkeys[name] = (MUL, left, right)
                elif words[2] == "/":
                    monkeys[name] = (DIV, left, right)
                else:
                    assert(False)
            else:
                assert(False)
        return monkeys

def calc(monkey, monkeys):
    if monkey[0] == YELL:
        return monkey[1]

    op, left_name, right_name = monkey

    left = calc(monkeys[left_name], monkeys)
    right = calc(monkeys[right_name], monkeys)

    if op == ADD:
        return left + right
    elif op == SUB:
        return left - right
    elif op == MUL:
        return left * right
    elif op == DIV:
        return left // right

    assert(False)

def part1():
    monkeys = parse()
    root = monkeys["root"]
    return calc(root, monkeys)

class TestDay21(unittest.TestCase):
    def test_parse(self):
        monkeys = parse()
        if is_sample:
            self.assertEqual(15, len(monkeys))
        else:
            self.assertEqual(2177, len(monkeys))

if __name__ == '__main__':
    unittest.main(exit=False)
    print()

    res1 = part1()
    # assert(res1 == 33 if is_sample else res1 == 2301)
    print(f"Part 1: {res1}", "(sample)" if is_sample else "")

    # res2 = part2()
    # assert(res2 == 56 * 62 if is_sample else res2 == 10336)
    # print(f"Part 2: {res2}", "(sample)" if is_sample else "")
