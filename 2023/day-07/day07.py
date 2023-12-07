import sys, unittest
from collections import defaultdict
from enum import Enum

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
sys.argv  = sys.argv[:1] # strip args, they scare the unittest module
is_sample = filename == "sample.txt"

class CardType(Enum):
    FIVE       = 0
    FOUR       = 1
    FULL_HOUSE = 2
    THREE      = 3
    TWO_PAIRS  = 4
    PAIR       = 5
    HIGH_CARD  = 6

class Card:
    def __init__(self, card_str, bid_str):
        self.card   = card_str
        self.values = [card_to_int(c) for c in card_str]
        self.rank   = rank(self.values)
        self.bid    = int(bid_str)

    def __repr__(self):
        return f"{self.card} ({self.rank}) [{self.bid}]"

    def __eq__(self, other):
        return self.card == other.card and self.bid == other.bid

    def __lt__(self, other):
        if self.rank.value < other.rank.value:
            return True
        if self.rank.value > other.rank.value:
            return False
        return self.values > other.values

def parse_cards():
    with open(filename) as f:
        lines = [x.split() for x in f.readlines()]
    cards = [Card(x[0], x[1]) for x in lines]
    return cards

def card_to_int(card):
    if card == 'T':
        return 10
    if card == 'J':
        return 11
    if card == 'Q':
        return 12
    if card == 'K':
        return 13
    if card == 'A':
        return 14

    return int(card)

def rank(card):
    kinds = defaultdict(int)

    for c in card:
        kinds[c] += 1

    if 5 in kinds.values():
        return CardType.FIVE

    if 4 in kinds.values():
        return CardType.FOUR

    if 3 in kinds.values():
        if 2 in kinds.values():
            # 2 groups of cards implies 3/2
            return CardType.FULL_HOUSE
        return CardType.THREE

    if 2 in kinds.values():
        if len(kinds) == 3:
            # 3 groups of cards implies 2/2/1
            return CardType.TWO_PAIRS
        return CardType.PAIR

    return CardType.HIGH_CARD

def get_winnings(cards):
    sorted_cards = sorted(cards, reverse=True)

    total_winnings = 0

    for rank, card in enumerate(sorted_cards):
        winning = (rank + 1) * card.bid
        # print(rank+1, card, winning)
        total_winnings += winning

    return total_winnings

class TestDay07(unittest.TestCase):
    def test_card_to_int(self):
        self.assertEqual(card_to_int('2'), 2)
        self.assertEqual(card_to_int('9'), 9)
        self.assertEqual(card_to_int('T'), 10)
        self.assertEqual(card_to_int('J'), 11)
        self.assertEqual(card_to_int('Q'), 12)
        self.assertEqual(card_to_int('K'), 13)
        self.assertEqual(card_to_int('A'), 14)

    def test_rank_sample_cards(self):
        self.assertEqual(rank(list('32T3K')), CardType.PAIR)
        self.assertEqual(rank(list('T55J5')), CardType.THREE)
        self.assertEqual(rank(list('KK677')), CardType.TWO_PAIRS)
        self.assertEqual(rank(list('KTJJT')), CardType.TWO_PAIRS)
        self.assertEqual(rank(list('QQQJA')), CardType.THREE)

    def test_rank_sample_cards_as_ints(self):
        self.assertEqual(rank([3, 2, 10, 3, 13]), CardType.PAIR)
        self.assertEqual(rank([10, 5, 5, 11, 5]), CardType.THREE)
        self.assertEqual(rank([13, 13, 6, 7, 7]), CardType.TWO_PAIRS)
        self.assertEqual(rank([13, 10, 11, 11, 10]), CardType.TWO_PAIRS)
        self.assertEqual(rank([12, 12, 12, 11, 14]), CardType.THREE)

    def test_rank_remaining_kinds(self):
        self.assertEqual(rank(list('23456')), CardType.HIGH_CARD)
        self.assertEqual(rank(list('A7A7A')), CardType.FULL_HOUSE)
        self.assertEqual(rank(list('AAA7A')), CardType.FOUR)
        self.assertEqual(rank(list('99999')), CardType.FIVE)

    def test_rank_remaining_kinds_as_ints(self):
        self.assertEqual(rank([2, 3, 4, 5, 6]), CardType.HIGH_CARD)
        self.assertEqual(rank([14, 7, 14, 7, 14]), CardType.FULL_HOUSE)
        self.assertEqual(rank([14, 14, 14, 7, 14]), CardType.FOUR)
        self.assertEqual(rank([9, 9, 9, 9, 9]), CardType.FIVE)

    def test_sorting_sample_cards(self):
        sample_cards = [
            Card('32T3K', '765'),
            Card('T55J5', '684'),
            Card('KK677', '28'),
            Card('KTJJT', '220'),
            Card('QQQJA', '483'),
        ]

        sorted_cards = sorted(sample_cards, reverse=True)

        self.assertEqual(sorted_cards, [
            Card('32T3K', '765'),
            Card('KTJJT', '220'),
            Card('KK677', '28'),
            Card('T55J5', '684'),
            Card('QQQJA', '483'),
        ])

if __name__ == '__main__':
    unittest.main(exit=False)
    print()

    cards = parse_cards()

    res1 = get_winnings(cards)
    assert res1 == (6440 if is_sample else 248179786)
    print(f"Part 1: {res1}{' (sample)' if is_sample else ''}")

    res2 = None
    # assert res2 == (?? if is_sample else ???)
    print(f"Part 2: {res2}{' (sample)' if is_sample else ''}")
