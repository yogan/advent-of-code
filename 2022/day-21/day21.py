import unittest, sys

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

def calc(monkey, monkeys, expr=False):
    if monkey[0] == YELL:
        return monkey[1]

    op, left_name, right_name = monkey

    left  = calc(monkeys[left_name],  monkeys, expr)
    right = calc(monkeys[right_name], monkeys, expr)

    if expr:
        return [op, left, right]

    if op == ADD:
        return left + right
    elif op == SUB:
        return left - right
    elif op == MUL:
        return left * right
    elif op == DIV:
        return left // right

    assert(False)

def simplify(expr, guess=None):
    if expr == 'SECRET' and guess is not None:
        return guess

    if isinstance(expr, int) or expr == 'SECRET':
        return expr

    op, left, right = expr

    left  = simplify(left, guess)
    right = simplify(right, guess)

    if isinstance(left, int) and isinstance(right, int):
        if op == ADD:
            return left + right
        elif op == SUB:
            return left - right
        elif op == MUL:
            return left * right
        elif op == DIV:
            return left // right

    return (op, left, right)

def part1():
    monkeys = parse()
    root = monkeys["root"]
    return calc(root, monkeys)

def part2():
    monkeys = parse()

    human = monkeys["humn"]
    assert(human[0] == YELL)
    monkeys["humn"] = (YELL, "SECRET")

    root  = monkeys["root"]
    _, left_name, right_name = root

    left_expr  = simplify(calc(monkeys[left_name],  monkeys, expr=True))
    right_expr = simplify(calc(monkeys[right_name], monkeys, expr=True))

    if isinstance(left_expr, int):
        target   = left_expr
        to_solve = right_expr
    elif isinstance(right_expr, int):
        target   = right_expr
        to_solve = left_expr
    else:
        assert(False)

    window = 100 if is_sample else 10012312312312 # chosen by fair dice roll
    (guess1, guess2) = 1, window

    for step in range(420):
        assert(guess1 < guess2)

        res1 = simplify(left_expr, guess1)
        too_low1 = res1 < target

        res2 = simplify(left_expr, guess2)
        too_low2 = res2 < target

        if res1 == target:
            print("Found after", step + 1, 'steps of "clever" searching.')
            return guess1
        if res2 == target:
            print("Found after", step + 1, 'steps of "clever" searching.')
            return guess2

        if too_low1 and too_low2:
            # both too low, move up one window
            guess1 = guess2
            guess2 = guess1 + window
        elif too_low1 and not too_low2:
            # res is in [guess1, guess2], try lower half of that
            window = abs(guess1 - guess2) // 2
            guess1 = guess1 + 1
            guess2 = guess1 + window
        elif not too_low1 and too_low2:
            # res is in [guess2, guess1], try lower half of that
            window = abs(guess1 - guess2) // 2
            guess1 = min(guess1, guess2) + 1
            guess2 = min(guess1, guess2) + window
        else:
            # both too high
            guess1 = guess2 + 1
            guess2 = guess2 + window

    return None

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
    assert(res1 == 152 if is_sample else res1 == 85616733059734)
    print(f"Part 1: {res1}", "(sample)" if is_sample else "")
    print()

    res2 = part2()
    assert(res2 == 301 if is_sample else res2 == 3560324848168)
    print(f"Part 2: {res2}", "(sample)" if is_sample else "")
