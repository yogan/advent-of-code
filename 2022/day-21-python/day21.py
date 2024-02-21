import unittest, sys

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
sys.argv  = sys.argv[:1] # strip args, they scare the unittest module
is_sample = filename != "input.txt"

class Op:
    YELL = 'num'
    ADD  = '+'
    SUB  = '-'
    MUL  = '*'
    DIV  = '/'

def parse(filename=filename):
    with open(filename) as f:
        monkeys = {}
        for line in f.readlines():
            words = line.strip().split()
            name = words[0].replace(":", "")
            if len(words) == 2:
                num = int(words[1])
                monkeys[name] = (Op.YELL, num)
            elif len(words) == 4:
                left = words[1]
                op = words[2]
                right = words[3]
                monkeys[name] = (op, left, right)
            else:
                raise Exception(f"Unexpected number of words (${len(words)})")
        return monkeys

def math(op, left, right):
    match op:
        case Op.ADD:
            return left + right
        case Op.SUB:
            return left - right
        case Op.MUL:
            return left * right
        case Op.DIV:
            if (left % right) != 0:
                raise ValueError(f"Division of {left} by {right} is not an integer.")
            return left // right

    raise Exception(f"Unknown operation: {op}")

def calc(monkey, monkeys, expr=False):
    if monkey[0] == Op.YELL:
        return monkey[1]

    op, left_name, right_name = monkey

    left  = calc(monkeys[left_name],  monkeys, expr)
    right = calc(monkeys[right_name], monkeys, expr)

    if expr:
        return [op, left, right]
    else:
        return math(op, left, right)

def evaluate(expression, guess=None):
    if expression == 'SECRET' and guess is not None:
        return guess

    if isinstance(expression, int) or expression == 'SECRET':
        return expression

    op, left, right = expression

    left  = evaluate(left, guess)
    right = evaluate(right, guess)

    if isinstance(left, int) and isinstance(right, int):
        return math(op, left, right)

    return (op, left, right)

def safe_evaluate(expression, guess):
    res = None
    while res is None:
        try:
            res = evaluate(expression, guess)
        except ValueError:
            # retry until we get a clean integer division
            guess += 1
    return (res, guess)

def prepare(monkeys):
    _, left_name, right_name = monkeys["root"]

    left_expr  = evaluate(calc(monkeys[left_name],  monkeys, expr=True))
    right_expr = evaluate(calc(monkeys[right_name], monkeys, expr=True))

    if isinstance(left_expr, int):
        return (right_expr, left_expr)
    elif isinstance(right_expr, int):
        return (left_expr, right_expr)

    raise Exception("At least one side of the root must be a number.")

def search(expression, target):
    # Value chosen by fair dice roll.
    # More seriously, it's a value where for both the sample and the real input
    # a result is found after a reasonable number of steps (35 and 41, respectively).
    # The reason why we need "luck" by choosing a good starting value here is
    # that the safe_evaluate function can be very slow when it encounters a non
    # integer division, as it only increases the guess value by one until it
    # eventually succeeds.
    # If for different input data, the search does not succeed (it's limited to
    # 420 steps), we can either:
    #   - try with a different window size until we are lucky
    #   - disable the "safe" integer division stuff, but then we might encounter
    #     false results (which again can be "fixed" by a different window size)
    window = 1012312312312

    (guess1, guess2) = 1, window

    for step in range(420):
        assert(guess1 < guess2)

        (res1, guess1) = safe_evaluate(expression, guess1)
        (res2, guess2) = safe_evaluate(expression, guess2)

        if res1 == target:
            print("Found after", step + 1, 'steps of "clever" searching.')
            return guess1
        if res2 == target:
            print("Found after", step + 1, 'steps of "clever" searching.')
            return guess2

        too_low1 = res1 < target
        too_low2 = res2 < target

        match (too_low1, too_low2):
            case (True, True):
                # both too low, move up one window
                guess1 = guess2
                guess2 = guess1 + window
            case (True, False):
                # res is in [guess1, guess2], try lower half of that
                window = abs(guess1 - guess2) // 2
                guess1 = guess1 + 1
                guess2 = guess1 + window
            case (False, True):
                # res is in [guess2, guess1], try lower half of that
                window = abs(guess1 - guess2) // 2
                guess1 = min(guess1, guess2) + 1
                guess2 = min(guess1, guess2) + window
            case (False, False):
                # both too high
                guess1 = guess2 + 1
                guess2 = guess2 + window

    raise Exception(f"Failed to find solution after {step} steps.")

def part1():
    monkeys = parse()
    root = monkeys["root"]
    return calc(root, monkeys)

def part2():
    monkeys = parse()
    monkeys["humn"] = (Op.YELL, "SECRET")

    (unsolved_expression, target) = prepare(monkeys)
    return search(unsolved_expression, target)

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
