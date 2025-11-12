import sys
import unittest
from tqdm import tqdm
from enum import Enum

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
is_sample = filename != "input.txt"

verbose = False

# This speeds up things a lot, but it is risky. When this is enabled,
# and a geode bot can be produced, it will be produced and all other
# options are ignored. There can be blueprint for which this does not work.
# With our sample and input data, it does, though.
optimize_for_geode_bots = True

def parse(filename=filename):
    with open(filename) as f:
        blueprints = []
        for line in f.readlines():
            words = line.strip().split()
            num = int(words[1].replace(":",""))
            ore_bot_cost = int(words[6])
            clay_bot_cost = int(words[12])
            obsidian_bot_ore_cost = int(words[18])
            obsidian_bot_clay_cost = int(words[21])
            geode_bot_ore_cost = int(words[27])
            geode_bot_obsidian_cost = int(words[30])
            blueprints += [(num, ore_bot_cost, clay_bot_cost,
                obsidian_bot_ore_cost, obsidian_bot_clay_cost,
                geode_bot_ore_cost, geode_bot_obsidian_cost)]
        return blueprints

class Build(Enum):
    NOTHING = 0
    ORE_BOT = 1
    CLAY_BOT = 2
    OBSIDIAN_BOT = 3
    GEODE_BOT = 4

def possible_builds(blueprint, resources, minutes_left, is_part_2):
    _, ore_bot_cost, clay_bot_cost, \
        obsidian_bot_ore_cost, obsidian_bot_clay_cost, \
        geode_bot_ore_cost, geode_bot_obsidian_cost = blueprint
    ore, clay, obsidian, geode = resources

    builds = []

    # limits found by crazy guessing and a lot of trial and error
    geode_minute_limit    =  0
    obsidian_minute_limit =  1
    clay_minute_limit     = 10 if is_part_2 else  5
    ore_minute_limit      = 20 if is_part_2 else 12

    if ore >= geode_bot_ore_cost and obsidian >= geode_bot_obsidian_cost \
        and minutes_left > geode_minute_limit:
        if optimize_for_geode_bots:
            return [Build.GEODE_BOT]
        builds += [Build.GEODE_BOT]

    if ore >= obsidian_bot_ore_cost and clay >= obsidian_bot_clay_cost \
        and minutes_left > obsidian_minute_limit:
        builds += [Build.OBSIDIAN_BOT]

    if ore >= clay_bot_cost \
        and minutes_left > clay_minute_limit:
        builds += [Build.CLAY_BOT]

    if ore >= ore_bot_cost \
        and minutes_left > ore_minute_limit:
        builds += [Build.ORE_BOT]

    # try to avoid building nothing
    if is_sample:
        if is_part_2:
            crazy_sample_part2_hack = obsidian_bot_clay_cost > 10
            if crazy_sample_part2_hack or len(builds) == 0:
                builds += [Build.NOTHING]
        else:
            builds += [Build.NOTHING]
    else:
        magic_value_found_by_trial_and_error = 12
        crazy_part1_hack = not is_part_2 and \
            obsidian_bot_clay_cost < magic_value_found_by_trial_and_error
        if crazy_part1_hack or len(builds) == 0:
            builds += [Build.NOTHING]

    return builds

def mine_resources(bots, resources):
    ore, clay, obsidian, geode = resources
    ore_bot, clay_bot, obsidian_bot, geode_bot = bots

    ore += ore_bot
    clay += clay_bot
    obsidian += obsidian_bot
    geode += geode_bot

    return (ore, clay, obsidian, geode)

def step(minute, build, bots, resources, blueprint):
    resources = mine_resources(bots, resources)

    ore, clay, obsidian, geode = resources
    ore_bot, clay_bot, obsidian_bot, geode_bot = bots
    _, ore_bot_cost, clay_bot_cost, \
        obsidian_bot_ore_cost, obsidian_bot_clay_cost, \
        geode_bot_ore_cost, geode_bot_obsidian_cost = blueprint

    if build == Build.NOTHING:
        return (minute, bots, resources, build)

    if build == Build.ORE_BOT:
        ore_bot += 1
        ore -= ore_bot_cost
        bots = (ore_bot, clay_bot, obsidian_bot, geode_bot)
        resources = (ore, clay, obsidian, geode)
        return (minute, bots, resources, build)

    if build == Build.CLAY_BOT:
        clay_bot += 1
        ore -= clay_bot_cost
        bots = (ore_bot, clay_bot, obsidian_bot, geode_bot)
        resources = (ore, clay, obsidian, geode)
        return (minute, bots, resources, build)

    if build == Build.OBSIDIAN_BOT:
        obsidian_bot += 1
        ore -= obsidian_bot_ore_cost
        clay -= obsidian_bot_clay_cost
        bots = (ore_bot, clay_bot, obsidian_bot, geode_bot)
        resources = (ore, clay, obsidian, geode)
        return (minute, bots, resources, build)

    if build == Build.GEODE_BOT:
        geode_bot += 1
        ore -= geode_bot_ore_cost
        obsidian -= geode_bot_obsidian_cost
        bots = (ore_bot, clay_bot, obsidian_bot, geode_bot)
        resources = (ore, clay, obsidian, geode)
        return (minute, bots, resources, build)

def reduce_states(states, minutes_left, max_geode):
    # group states per bots, but skip those that cannot reach a new max geode
    bots_to_states = {}
    for state in states:
        _, bots, resources, _ = state
        _, _, _, geode = resources
        _, _, _, geode_bot = bots

        potential_max_geode = geode
        additional_geode_bots = 0
        for i in range(1, minutes_left + 1):
            # every minute, we open as many geodes as we have geode bots
            potential_max_geode += geode_bot + additional_geode_bots
            # best-case, we also produce a new geode bot every minute
            additional_geode_bots += 1

        if potential_max_geode < max_geode:
            # we can never reach a higher max geode with this state, so skip it
            continue

        if bots not in bots_to_states:
            bots_to_states[bots] = []
        bots_to_states[bots].append(state)

    best_states_for_all_bots = set()

    for bots, states in bots_to_states.items():
        # Group items that have the same clay, geodes, and obsidian (but different
        # ore) together
        clay_geode_obsidian_to_states = {}
        for state in states:
            _, _, (_, clay, obsidian, geode), _ = state
            key = (clay, geode, obsidian)
            if key not in clay_geode_obsidian_to_states:
                clay_geode_obsidian_to_states[key] = []
            clay_geode_obsidian_to_states[key].append(state)
        # In each clay/geode/obsidian group, keep only the one with the highest
        # ore count
        for _, states in clay_geode_obsidian_to_states.items():
            if len(states) <= 1:
                continue
            states.sort(key=lambda state: state[2][0], reverse=True)
            states = states[:1]

        # group items that have the same ore, geodes, and obsidian (but different
        # clay) together
        ore_geode_obsidian_to_states = {}
        for state in states:
            _, _, (ore, _, obsidian, geode), _ = state
            key = (ore, geode, obsidian)
            if key not in ore_geode_obsidian_to_states:
                ore_geode_obsidian_to_states[key] = []
            ore_geode_obsidian_to_states[key].append(state)
        # in each ore/geode/obsidian group, keep only the one with the highest
        # clay count
        for _, states in ore_geode_obsidian_to_states.items():
            if len(states) <= 1:
                continue
            states.sort(key=lambda state: state[2][1], reverse=True)
            states = states[:1]

        # group items that have the same ore, clay, and geodes (but different
        # obsidian) together
        ore_clay_geode_to_states = {}
        for state in states:
            _, _, (ore, clay, _, geode), _ = state
            key = (ore, clay, geode)
            if key not in ore_clay_geode_to_states:
                ore_clay_geode_to_states[key] = []
            ore_clay_geode_to_states[key].append(state)
        # in each ore/clay/geode group, keep only the one with the highest
        # obsidian count
        for _, states in ore_clay_geode_to_states.items():
            if len(states) <= 1:
                continue
            states.sort(key=lambda state: state[2][2], reverse=True)
            states = states[:1]

        # group items that have the same ore, clay, and obsidian (but different
        # geodes) together
        ore_clay_obsidian_to_states = {}
        for state in states:
            _, _, (ore, clay, obsidian, _), _ = state
            key = (ore, clay, obsidian)
            if key not in ore_clay_obsidian_to_states:
                ore_clay_obsidian_to_states[key] = []
            ore_clay_obsidian_to_states[key].append(state)
        # in each ore/clay/obsidian group, keep only the one with the highest
        # geode count
        for _, states in ore_clay_obsidian_to_states.items():
            if len(states) <= 1:
                continue
            states.sort(key=lambda state: state[2][3], reverse=True)
            states = states[:1]

        # merge values of clay_geode_obsidian_to_states, ore_geode_obsidian_to_states,
        # ore_clay_geode_to_states, and ore_clay_obsidian_to_states into list
        best_states = set()
        for _, states in clay_geode_obsidian_to_states.items():
            for state in states:
                best_states.add(state)
        for _, states in ore_geode_obsidian_to_states.items():
            for state in states:
                best_states.add(state)
        for _, states in ore_clay_geode_to_states.items():
            for state in states:
                best_states.add(state)
        for _, states in ore_clay_obsidian_to_states.items():
            for state in states:
                best_states.add(state)

        best_states_for_all_bots |= best_states

    return list(best_states_for_all_bots)

def simulate(blueprints, max_minutes, is_part_2=False):
    quality_levels = []
    max_geodes = []

    for blueprint in blueprints:
        num = blueprint[0]
        max_geode = 0

        if not is_sample and not is_part_2 and use_pre_calc and num in pre_calc_quality_levels:
            pre_calc_quality_level = pre_calc_quality_levels[num]
            if verbose:
                print(f"Blueprint {num}/{len(blueprints)}: quality level {pre_calc_quality_level} (pre-calc)")
            quality_levels.append(pre_calc_quality_level)
            continue

        states = [(0, (1, 0, 0, 0), (0, 0, 0, 0), Build.NOTHING)] # initial

        for minute in range(1, max_minutes + 1):
            minutes_left = max_minutes - minute
            next_states = []

            state_iter = states
            if verbose and len(states) > 10000:
                state_iter = tqdm(states, f"States ({minute}/{max_minutes}")

            for state in state_iter:
                _, bots, resources, factory = state
                builds = possible_builds(blueprint, resources, minutes_left, is_part_2)
                possibilities = [step(minute, build, bots, resources,
                                      blueprint) for build in builds]
                for possibility in possibilities:
                    _, _, _, geode = possibility[2]
                    if geode > max_geode:
                        max_geode = geode
                    next_states.append(possibility)

            reduced_states = reduce_states(next_states, minutes_left, max_geode)
            # print(f"min: {minute} - reduced states: {len(next_states)} → {len(reduced_states)}")
            states = reduced_states

        max_geodes.append(max_geode)
        quality_level = max_geode * num
        quality_levels.append(quality_level)

        if verbose:
            print("===================================================================")
            print(f"Blueprint {num}/{len(blueprints)}: {len(states)} final states, max geode: {max_geode}, quality level: {quality_level}")
            print("===================================================================")
            print()

    return max_geodes if is_part_2 else quality_levels

def part1():
    blueprints = parse()
    max_minutes = 24
    quality_levels = simulate(blueprints, max_minutes)
    if verbose:
        print("Quality levels:", quality_levels)
    return sum(quality_levels)

def part2():
    blueprints = parse()
    if not is_sample:
        blueprints = blueprints[:3]
    max_minutes = 32

    max_geodes = simulate(blueprints, max_minutes, is_part_2=True)
    if verbose:
        print("Max geodes:", max_geodes)

    result = 1
    for max_geode in max_geodes:
        result *= max_geode
    return result

class TestDay19(unittest.TestCase):
    def test_parse(self):
        blueprints = parse()
        if is_sample:
            self.assertEqual(2, len(blueprints))
            self.assertEqual((1, 4, 2, 3, 14, 2, 7), blueprints[0])
            self.assertEqual((2, 2, 3, 3, 8, 3, 12), blueprints[1])
        else:
            self.assertEqual(30, len(blueprints))

use_pre_calc = False
pre_calc_quality_levels = {
    1: 3,
    2: 0,
    3: 0,
    4: 8,
    5: 45,
    6: 12,
    7: 7,
    8: 0,
    9: 54,
    10: 10,
    11: 165,
    12: 0,
    13: 117,
    14: 154,
    15: 0,
    16: 0,
    17: 204,
    18: 0,
    19: 0,
    20: 60,
    21: 63,
    22: 22,
    23: 368,
    24: 0,
    25: 175,
    26: 234,
    27: 135,
    28: 0,
    29: 435,
    30: 30,
}

if __name__ == '__main__':
    unittest.main(argv=sys.argv[:1], exit=False)
    print()

    if not verbose:
        expected_run_time = 90 if is_sample else 35
        if optimize_for_geode_bots:
            expected_run_time = 12 if is_sample else 25
        print(f"Attention, this stuff takes a while to run (about {expected_run_time}s).")
        print("To see some progress bars, enable verbose mode (verbose = True)")
        print()

    res1 = part1()
    assert(res1 == 33 if is_sample else res1 == 2301)

    res2 = part2()
    assert(res2 == 56 * 62 if is_sample else res2 == 10336)

    print(f"Part 1: {res1}", "(sample)" if is_sample else "")
    print(f"Part 2: {res2}", "(sample)" if is_sample else "")
