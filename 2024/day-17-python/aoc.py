import sys
import unittest


def parse(filename):
    reg_str, prog_str = open(filename).read().strip().split("\n\n")
    regs = [int(line.strip().split(": ")[1]) for line in reg_str.split("\n")]
    prog = [int(op) for op in prog_str.strip().split(": ")[1].split(",")]
    return prog, {"A": regs[0], "B": regs[1], "C": regs[2]}


def run(prog, regs):
    output = []
    ip = 0

    while ip < len(prog) - 1:  # -1 for op
        instr = prog[ip]
        op = prog[ip + 1]

        if instr == 0:
            # adv: division
            numerator = regs["A"]
            denominator = 2 ** combo(op, regs)
            regs["A"] = numerator // denominator
        elif instr == 1:
            # bxl: bitwise XOR
            regs["B"] = regs["B"] ^ op
        elif instr == 2:
            # bst: combo mod 8
            regs["B"] = combo(op, regs) % 8
        elif instr == 3:
            # jnz: jump if not zero
            if regs["A"] != 0:
                ip = op
                continue
        elif instr == 4:
            # bxc: bitwise XOR
            regs["B"] = regs["B"] ^ regs["C"]
        elif instr == 5:
            # out: output
            output.append(combo(op, regs) % 8)
        elif instr == 6:
            # bdv: division
            numerator = regs["A"]
            denominator = 2 ** combo(op, regs)
            regs["B"] = numerator // denominator
        elif instr == 7:
            # cdv: division
            numerator = regs["A"]
            denominator = 2 ** combo(op, regs)
            regs["C"] = numerator // denominator

        ip += 2

    return output


def combo(op, regs):
    if op < 4:
        return op
    elif op == 4:
        return regs["A"]
    elif op == 5:
        return regs["B"]
    elif op == 6:
        return regs["C"]
    else:
        raise ValueError(f"Invalid combo operand {op}")


class Tests(unittest.TestCase):
    pass


if __name__ == "__main__":
    flags = set(arg for arg in sys.argv[1:] if arg.startswith("-"))
    args = [arg for arg in sys.argv[1:] if not arg.startswith("-")]
    sys.argv = sys.argv[:1]  # strip args, unittest.main() doesn't like them

    filename = "sample.txt" if "-s" in flags or "--sample" in flags else "input.txt"
    filename = args[0] if args else filename
    is_sample = filename.startswith("sample")
    run_tests = "-t" in flags or "--test" in flags

    if run_tests:
        unittest.main(exit=True)

    def check(part, actual, expected=None):
        print(f"Part {part}{' (sample)' if is_sample else ''}: {actual} ", end="")
        if expected is None:
            print("❔")
        else:
            if actual != expected:
                print(f"≠ {expected} ❌")
                exit(1)
            print("✅")

    prog, regs = parse(filename)
    part1 = ",".join(str(n) for n in run(prog, regs))
    part2 = None

    check(1, part1, "4,6,3,5,6,3,5,2,1,0" if is_sample else "3,6,7,0,5,7,3,1,4")
    check(2, part2)
