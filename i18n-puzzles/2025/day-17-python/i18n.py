import re
import sys
import unittest


def parse():
    return [
        [bytes.fromhex(line) for line in block.split("\n")]
        for block in open(filename).read().strip().split("\n\n")
    ]


def analyze(index, block):
    H, W = len(block), len(block[0])
    assert W == 16
    corner = None
    incomplete_left = []
    incomplete_right = []

    for i, line in enumerate(block):
        text = line.decode("utf-8", errors="replace")
        # print(text)
        if i == 0:
            if text.startswith("╔"):
                corner = "tl"
            elif text.endswith("╗"):
                corner = "tr"
        elif i == H - 1:
            if text.startswith("╚"):
                corner = "bl"
            elif text.endswith("╝"):
                corner = "br"

        leading_replacements = re.search(r"^�+", text)
        if leading_replacements:
            incomplete_left.append([i, line])

        trailing_replacements = re.search(r"�+$", text)
        if trailing_replacements:
            incomplete_right.append([i, line])

    return {
        "index": index,
        "lines": block,
        "corner": corner,
        "h": H,
        "w": W,
        "incomplete_left": incomplete_left,
        "incomplete_right": incomplete_right,
    }


def trim(block, offset):
    trimmed = block.copy()
    trimmed["lines"] = block["lines"][offset:]
    trimmed["h"] = block["h"] - offset
    assert trimmed["h"] == len(trimmed["lines"])
    trimmed["incomplete_left"] = [
        [i - offset, line] for i, line in block["incomplete_left"] if i - offset >= 0
    ]
    trimmed["incomplete_right"] = [
        [i - offset, line] for i, line in block["incomplete_right"] if i - offset >= 0
    ]
    # print("untrimmed")
    # debug_print(block)
    # print("trimmed")
    # debug_print(trimmed)
    return trimmed


def fits(left, right):
    height = min(left["h"], right["h"])

    l_pairs = [[i, bytes] for [i, bytes] in left["incomplete_right"] if i < height]
    r_pairs = [[i, bytes] for [i, bytes] in right["incomplete_left"] if i < height]

    l_indices = [i for i, _ in l_pairs]
    r_indices = [i for i, _ in r_pairs]

    # no need for complicated shit if the broken lines don't even match up
    if l_indices != r_indices:
        return False

    # print("possible candidate", right["index"], r_pairs)

    # to fit together, every entry of il must be matched to an entry of ir
    # and the combination of the bytes must be able to be decoded to utf-8
    for i, left_bytes in l_pairs:
        rights = [right_bytes for j, right_bytes in r_pairs if i == j]
        assert len(rights) < 2
        if not rights:
            return False
        right_bytes = rights[0]
        byte_seq = left_bytes + right_bytes
        text = byte_seq.decode("utf-8", errors="replace")
        # print(byte_seq.hex(), text)
        hex_middle = re.sub(r"^(efbfbd)*|(efbfbd)*$", "", text.encode().hex())
        if "efbfbd" in hex_middle:
            return False

    return True


def is_left_border(block):
    return block["incomplete_left"] == []


def is_right_border(block):
    return block["incomplete_right"] == []


# TODO return x/y coordinates of the x
def has_x(block):
    return any("╳" in line.decode("utf-8", errors="replace") for line in block["lines"])


def debug_print(block):
    print(
        block["lines"][0].decode("utf-8", errors="replace"),
        (
            "left  "
            if is_left_border(block)
            else ("right " if is_right_border(block) else "middle")
        ),
        "X" if has_x(block) else "-",
        "r/c:",
        block["r"],
        block["c"],
        "h/w:",
        block["h"],
        block["w"],
        # list(map(lambda x: x[0], block["incomplete_left"])),
        # list(map(lambda x: x[0], block["incomplete_right"])),
    )


def calc_offset(found, curr_height, next_c):
    next_column_blocks = [block for block in found if block["c"] == next_c]

    if next_column_blocks == []:
        return 0, 0

    next_max = max(next_column_blocks, key=lambda x: x["h"])
    next_height = next_max["h"] + next_max["r"]
    offset = next_height - curr_height

    print("offset", offset, "r", next_height)
    return offset, next_height


def solve(blocks):
    analysis = [analyze(i, block) for i, block in enumerate(blocks)]
    found = []
    direction = "RIGHT"

    cur = next(a for a in analysis if a["corner"] == "tl")
    analysis.remove(cur)
    assert cur["incomplete_left"] == []
    assert cur["incomplete_right"] != []

    cur["r"] = 0
    cur["c"] = 0
    found.append(cur)
    debug_print(cur)

    r, c = 0, 0

    while analysis:
        c = c + cur["w"] if direction == "RIGHT" else c - cur["w"]
        offset, r = calc_offset(found, cur["r"], c)
        trimmed_block = trim(cur, offset) if offset else cur

        if direction == "RIGHT":
            candidates = [a for a in analysis if fits(trimmed_block, a)]
        else:
            candidates = [a for a in analysis if fits(a, trimmed_block)]

        if not candidates:
            print("no candidates found")
            break
        assert len(candidates) == 1, len(candidates)

        cur = candidates[0]
        cur["r"] = r
        cur["c"] = c

        if has_x(cur):
            print("!!! FOUND X !!!")

        analysis.remove(cur)
        found.append(cur)
        debug_print(cur)

        if direction == "LEFT" and is_left_border(cur):
            direction = "RIGHT"
            print("change direction to RIGHT")
        elif direction == "RIGHT" and is_right_border(cur):
            direction = "LEFT"
            print("change direction to LEFT")


class Tests(unittest.TestCase):
    pass
    # def test_to_ranges(self):
    #     self.assertEqual(to_ranges([1, 2, 3, 4, 7, 8, 9, 10]), [(1, 4), (7, 10)])


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

    def check(actual, expected=None):
        print(f"Result{' (sample)' if is_sample else ''}: {actual} ", end="")
        if expected is None:
            print("❔")
        else:
            if actual != expected:
                print(f"≠ {expected} ❌")
                exit(1)
            print("✅")

    blocks = parse()
    solve(blocks)

    # 8 blocks in sample.txt
    # 200 blocks in input.txt
    res = len(blocks)

    # ╳ is in the non-broken / split up area, no need to repair
    # both for sample.txt and input.txt

    check(res, 132 if is_sample else None)
