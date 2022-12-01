import unittest
from input import read_and_solve


def parse_input(lines):
    pos_1 = int(lines[0].split(": ")[1])
    pos_2 = int(lines[1].split(": ")[1])
    return [pos_1, pos_2]


def part1(lines):
    positions = list(map(lambda x: x - 1, parse_input(lines)))
    scores = [0, 0]
    player_index = 0
    rolls = 0
    dice = 0

    while all(map(lambda s: s < 1000, scores)):
        movement = 0
        for _ in range(3):
            dice += 1
            if dice == 101:
                dice = 1
            movement += dice

        rolls += 3

        positions[player_index] = (positions[player_index] + movement) % 10
        scores[player_index] += positions[player_index] + 1

        player_index = (player_index + 1) % len(scores)

    return rolls * min(scores)


def part2(lines):
    positions = list(map(lambda x: x - 1, parse_input(lines)))

    moves = {
        # steps: # of universes
        3: 1,
        4: 3,
        5: 6,
        6: 7,
        7: 6,
        8: 3,
        9: 1,
    }

    players = [
        [(0, positions[0], 1)],
        [(0, positions[1], 1)],
    ]

    counters = [0, 0]

    while players[0] and players[1]:
        for index in range(len(players)):
            tuples = []
            # print(players[index])

            for score, position, universes in players[index]:
                new_positions = {}  # {new_pos: #_univ}

                for steps, next_universes in moves.items():
                    new_position = (position + steps) % 10
                    new_positions[new_position] = universes * next_universes

                new_tuples = [(score + pos + 1, pos, univ)
                              for pos, univ in new_positions.items()]

                winning = [t for t in new_tuples if t[0] >= 21]
                non_winning = [t for t in new_tuples if t[0] < 21]

                win_universes = sum([t[2] for t in winning])
                other_player_index = (index + 1) % len(players)
                other_player_universes = sum(
                    [t[2] for t in players[other_player_index]])
                counters[index] += win_universes * other_player_universes

                tuples.extend(non_winning)

            players[index] = tuples

    # print(counters)
    return max(counters)


class TestDay21(unittest.TestCase):

    sample = [
        "Player 1 starting position: 4",
        "Player 2 starting position: 8",
    ]

    def test_parse_input(self):
        [pos_1, pos_2] = parse_input(self.sample)
        self.assertEqual(pos_1, 4)
        self.assertEqual(pos_2, 8)

    def test_part_1_sample(self):
        self.assertEqual(part1(self.sample), 739785)

    def test_part_2_sample(self):
        self.assertEqual(part2(self.sample), 444356092776315)


if __name__ == '__main__':
    unittest.main(exit=False)
    read_and_solve(__file__, part1, part2)
