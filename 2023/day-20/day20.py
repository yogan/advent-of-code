import sys, unittest
from collections import defaultdict, deque
from enum import Enum
from math import gcd

if len(sys.argv) < 2:
    print("Missing input file.")
    exit(1)
filename  = sys.argv[1]
if len(sys.argv) > 2:
    base, ext = filename.rsplit(".", 1)
    filename = base + sys.argv[2] + "." + ext
sys.argv  = sys.argv[:1] # strip args, they confuse the unittest module
is_sample = filename.startswith("sample")

class Type(Enum):
    FlipFlop    = '%'
    Conjunction = '&'
    Broadcast   = '*'

class Module:
    def __init__(self, line):
        left, right = line.split(" -> ")
        self.outputs = right.split(", ")

        if left[0] == Type.FlipFlop.value:
            self.type = Type.FlipFlop
            self.name = left[1:]
            self.on   = False
        elif left[0] == Type.Conjunction.value:
            self.type = Type.Conjunction
            self.name = left[1:]
            self.inputs = defaultdict(lambda: False)
        else:
            assert left == "broadcaster", f"Unknown type: {left}"
            self.type = Type.Broadcast
            self.name = left

    def __repr__(self):
        out = ", ".join(self.outputs)
        name = self.type.value + self.name
        state = ""

        if self.type == Type.FlipFlop:
            state = "ON" if self.on else "OFF"
        elif self.type == Type.Conjunction:
            state = ",".join(f"{k}={v}" for k, v in self.inputs.items())

        return f"{name}[{state}] → {out}"

    def receive(self, pulse, source):
        # uhm, yeah, "polymorphism"…
        if self.type == Type.FlipFlop:
            if pulse == 1:
                return []
            self.on = not self.on
            return [Pulse(self.name, out, 1 if self.on else 0) for out in self.outputs]

        if self.type == Type.Conjunction:
            self.inputs[source] = pulse
            all_high = all(self.inputs.values())
            return [Pulse(self.name, out, 0 if all_high else 1) for out in self.outputs]

        if self.type == Type.Broadcast:
            return [Pulse(self.name, out, pulse) for out in self.outputs]

        assert False, f"Unknown type: {self.type}"

class Pulse:
    def __init__(self, source, target, value):
        self.source = source
        self.target = target
        self.value  = value

    def __repr__(self):
        return f"{self.source} —{self.value}→ {self.target}"

def parse(file=filename):
    modules = dict()
    conjunctions = []

    for mod in [Module(line.strip()) for line in open(file).readlines()]:
        modules[mod.name] = mod
        if mod.type == Type.Conjunction:
            conjunctions.append(mod)

    for conj in conjunctions:
        inputs = [mod for mod in modules.values() if conj.name in mod.outputs]
        for inp in inputs:
            conj.inputs[inp.name] = 0

    return modules

def push_button(modules, names_to_observe=[]):
    queue = deque([Pulse("button", "broadcaster", 0)])
    low   = 1  # initial button push
    high  = 0

    activated_observers = defaultdict(lambda: False)

    while queue:
        pulse = queue.popleft()

        if pulse.target not in modules:
            continue

        new_pulses = modules[pulse.target].receive(pulse.value, pulse.source)

        for new_pulse in new_pulses:
            if new_pulse.value == 0:
                low += 1
                if new_pulse.target in names_to_observe:
                    activated_observers[new_pulse.target] = True
            else:
                high += 1
            queue.append(new_pulse)

    return low, high, activated_observers

def part1():
    modules = parse()
    total_low, total_high = 0, 0

    for _ in range(1000):
        low, high, _ = push_button(modules)
        total_low  += low
        total_high += high

    return total_low * total_high

def assert_has_rx(modules):
    has_rx = "rx" in [o for m in modules.values() for o in m.outputs]

    if is_sample:
        print("Part 2 (sample): n/a")
        assert not has_rx, "unexpected rx module in sample"
        exit(0)

    assert has_rx, "no rx module"

def find_circle_outputs(modules):
    """
    Plotting the module as a graph reveals a very specific pattern:

    ```
                 ┌─ cl ← rl ← CIRCLE1
        rx ← lx ←┼─ rp ← rd ← CIRCLE2
                 ├─ lb ← qb ← CIRCLE3
                 └─ nj ← nn ← CIRCLE4
    ```

    All modules (besides `rx`) shown above are conjunctions.

    The modules `rl`, `rd`, `qb`, and `nn` are conntected to circle-like
    subgraphs, both with other in- and outputs. However, their output is what
    eventually controls what pulses are sent to `rx`.

    Let's at least assert this structure, before we fire off an algo that
    relies on those assumptions.

    The direct inputs of `lx` are what we need to observe, the are returned
    after the assertions.
    """

    rx_pre = [mod for mod in modules.values() if "rx" in mod.outputs]
    assert len(rx_pre) == 1
    rx_pre = rx_pre[0]
    assert rx_pre.name == "lx" and rx_pre.type == Type.Conjunction

    lx_pre = [mod for mod in modules.values() if "lx" in mod.outputs]
    assert set([mod.name for mod in lx_pre]) == set(["cl", "rp", "lb", "nj"])
    assert all(mod.type == Type.Conjunction for mod in lx_pre)

    lx_pre_pre = []
    for lp in lx_pre:
        lpp = [mod for mod in modules.values() if lp.name in mod.outputs]
        assert len(lpp) == 1
        lx_pre_pre.append(lpp[0])
    assert set([mod.name for mod in lx_pre_pre]) == set(["rl", "rd", "qb", "nn"])
    assert all(mod.type == Type.Conjunction for mod in lx_pre_pre)

    return lx_pre

def lcm(*args):
    res = args[0]
    for arg in args[1:]:
        res = res * arg // gcd(res, arg)
    return res

def part2(modules):
    """
    As checked in `find_circle_outputs()`, our machine looks like this:

    ```
                 ┌─ cl ← rl ← CIRCLE1
        rx ← lx ←┼─ rp ← rd ← CIRCLE2
                 ├─ lb ← qb ← CIRCLE3
                 └─ nj ← nn ← CIRCLE4
    ```

    The `lx_pre` modules (`cl`, `rp`, `lb`, `nj`) are what we need to observe.

    The machine turns on when `rx` receives a low pulse, which happens when `lx`
    sends a high pulse, which itself happens when all `lx_pre` modules are high,
    and that finally happens when all of their inputs are low - at the same
    time!

    So, run the whole machine and observe the `lx_pre` modules. We remember the
    number of button presses until an `lx_pre` module gets a low pulse for the
    first time. This is the cycle length for that module.

    With all cycle lengths known, we can calculate the least common multiple
    (LCM) of all cycle lengths. This is the number of button presses until `rx`
    receives a low pulse, and the machine turns on.
    """

    lx_pre_names = [mod.name for mod in find_circle_outputs(modules)]

    cycle_length = {}

    for presses in range(1, 1_000_000_000_000): # :-)
        if len(cycle_length) == len(lx_pre_names):
            break

        _, _, activations = push_button(modules, lx_pre_names)

        for name, activated in activations.items():
            if activated and name not in cycle_length:
                cycle_length[name] = presses

    return lcm(*cycle_length.values())

# --- TESTS --------------------------------------------------------------------
class TestDay20(unittest.TestCase):
    def test_sample1_parsed_as_expected(self):
        modules = parse("sample1.txt")

        self.assertEqual(len(modules), 5)

        self.assertEqual(modules["broadcaster"].type, Type.Broadcast)
        self.assertEqual(set(modules["broadcaster"].outputs), {"a", "b", "c"})

        self.assertEqual(modules["a"].type, Type.FlipFlop)
        self.assertEqual(modules["b"].type, Type.FlipFlop)
        self.assertEqual(modules["c"].type, Type.FlipFlop)
        self.assertFalse(modules["a"].on)
        self.assertFalse(modules["b"].on)
        self.assertFalse(modules["c"].on)
        self.assertEqual(set(modules["a"].outputs), {"b"})
        self.assertEqual(set(modules["b"].outputs), {"c"})
        self.assertEqual(set(modules["c"].outputs), {"inv"})

        self.assertEqual(modules["inv"].type, Type.Conjunction)
        self.assertEqual(set(modules["inv"].inputs.keys()), {"c"})
        self.assertEqual(modules["inv"].inputs["c"], 0)
        self.assertEqual(set(modules["inv"].outputs), {"a"})

    def test_sample2_first_button_push(self):
        modules = parse("sample2.txt")

        self.assertEqual(len(modules), 5)

        self.assertEqual(modules["broadcaster"].type, Type.Broadcast)
        self.assertEqual(set(modules["broadcaster"].outputs), {"a"})

        self.assertEqual(modules["a"].type, Type.FlipFlop)
        self.assertEqual(modules["b"].type, Type.FlipFlop)
        self.assertFalse(modules["a"].on)
        self.assertFalse(modules["b"].on)
        self.assertEqual(set(modules["a"].outputs), {"inv", "con"})
        self.assertEqual(set(modules["b"].outputs), {"con"})

        self.assertEqual(modules["inv"].type, Type.Conjunction)
        self.assertEqual(set(modules["inv"].inputs.keys()), {"a"})
        self.assertEqual(modules["inv"].inputs["a"], 0)
        self.assertEqual(set(modules["inv"].outputs), {"b"})

        self.assertEqual(modules["con"].type, Type.Conjunction)
        self.assertEqual(set(modules["con"].inputs.keys()), {"a", "b"})
        self.assertEqual(modules["con"].inputs["a"], 0)
        self.assertEqual(modules["con"].inputs["b"], 0)
        self.assertEqual(set(modules["con"].outputs), {"output"})

    def test_sample1_single_button_push(self):
        modules = parse("sample1.txt")
        low, high, _ = push_button(modules)

        self.assertEqual(low,  8)
        self.assertEqual(high, 4)

    def test_sample1_thousand_button_pushes(self):
        modules = parse("sample1.txt")
        total_low, total_high = 0, 0

        for _ in range(1000):
            low, high, _ = push_button(modules)
            total_low  += low
            total_high += high

        self.assertEqual(total_low,  8000)
        self.assertEqual(total_high, 4000)

    def test_sample2_thousand_button_pushes(self):
        modules = parse("sample2.txt")
        total_low, total_high = 0, 0

        for _ in range(1000):
            low, high, _ = push_button(modules)
            total_low  += low
            total_high += high

        self.assertEqual(total_low,  4250)
        self.assertEqual(total_high, 2750)

# --- MAIN ---------------------------------------------------------------------
def check(part, actual, expected=None):
    print(f"Part {part}{' (sample)' if is_sample else ''}: {actual} ", end="")
    if expected is None:
        print("❔")
    else:
        if actual != expected:
            print(f"≠ {expected} ❌")
            exit(1)
        print("✅")

if __name__ == '__main__':
    if is_sample:
        unittest.main(exit=False)
        print()

    expected_part1 = {
        "sample1.txt":  32000000,
        "sample2.txt":  11687500,
        "input.txt":   794930686,
    }
    check(1, part1(), expected_part1[filename])

    modules = parse()
    assert_has_rx(modules)  # will exit for samples
    check(2, part2(modules), 244465191362269)
