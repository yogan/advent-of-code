import sys


def parse(filename):
    reg_str, prog_str = open(filename).read().strip().split("\n\n")
    regs = [int(line.strip().split(": ")[1]) for line in reg_str.split("\n")]
    prog = [int(op) for op in prog_str.strip().split(": ")[1].split(",")]
    return prog, *regs


def run(prog, a, b, c):
    output = []
    ip = 0

    def combo(op):
        if op < 4:
            return op
        elif op == 4:
            return a
        elif op == 5:
            return b
        elif op == 6:
            return c
        else:
            raise ValueError(f"Invalid combo operand {op}")

    while ip < len(prog) - 1:
        instr = prog[ip]
        op = prog[ip + 1]

        if instr == 0:
            a = a >> combo(op)
        elif instr == 1:
            b = b ^ op
        elif instr == 2:
            b = combo(op) % 8
        elif instr == 3 and a != 0:
            ip = op
            continue
        elif instr == 4:
            b = b ^ c
        elif instr == 5:
            output.append(combo(op) % 8)
        elif instr == 6:
            b = b >> combo(op)
        elif instr == 7:
            c = a >> combo(op)

        ip += 2

    return output


# This is not a general solution, but specific for my input.
# See input-decompile.txt for comments on what my program roughly does.
# The main point is that it is a loop, where for each iteration a single digit
# is calculated and outputted. The output value depends on the lowest 3 bits of
# a, which is shifted right by 3 bits in each iteration. Therefore, we can find
# the target value by iterating through all 3-bit values recursively.
def find_quine(goal, res=0):
    if goal == []:
        return res

    for a_lo in range(8):
        a = res << 3 | a_lo
        b = a % 8
        b = b ^ 1
        c = a >> b
        b = b ^ 0b101
        b = b ^ c
        if b % 8 == goal[-1]:
            next = find_quine(goal[:-1], a)
            if next:
                return next


if __name__ == "__main__":
    flags = set(arg for arg in sys.argv[1:] if arg.startswith("-"))
    args = [arg for arg in sys.argv[1:] if not arg.startswith("-")]

    filename = "sample.txt" if "-s" in flags or "--sample" in flags else "input.txt"
    filename = args[0] if args else filename
    is_sample = filename.startswith("sample")

    def check(part, actual, expected=None):
        print(f"Part {part}{' (sample)' if is_sample else ''}: {actual} ", end="")
        if expected is None:
            print("❔")
        else:
            if actual != expected:
                print(f"≠ {expected} ❌")
                exit(1)
            print("✅")

    prog, a, b, c = parse(filename)
    part1 = ",".join(str(n) for n in run(prog, a, b, c))
    part2 = "n/a" if is_sample else find_quine(prog)

    check(1, part1, "4,6,3,5,6,3,5,2,1,0" if is_sample else "3,6,7,0,5,7,3,1,4")
    check(2, part2, "n/a" if is_sample else 164278496489149)
