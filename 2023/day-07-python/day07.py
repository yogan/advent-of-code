import sys, unittest
from collections import defaultdict
from enum import Enum

if len(sys.argv) != 2:
    print("Missing input file.")
    sys.exit(1)
filename  = sys.argv[1]
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

    def rerank_with_joker(self):
        self.values = [x if x != 11 else 1 for x in self.values]

        remaining_cards = [c for c in self.values if c != 1]
        jokers = 5 - len(remaining_cards)

        if jokers == 0 or self.rank == CardType.FIVE:
            return

        if jokers == 4 or jokers == 5:
            self.rank = CardType.FIVE
            return

        if jokers == 3:
            # if remaing two cards are the same, we can make a 5 of a kind
            if remaining_cards[0] == remaining_cards[1]:
                self.rank = CardType.FIVE
            else:
                self.rank = CardType.FOUR
            return

        non_joker_values = [x for x in range(2, 15) if x != 1]

        if jokers == 2:
            # just brute force it
            for first_joker in non_joker_values:
                for second_joker in non_joker_values:
                    new_rank = rank(remaining_cards + [first_joker, second_joker])
                    if new_rank.value < self.rank.value:
                        self.rank = new_rank
            return

        if jokers == 1:
            # lil brute force
            for joker in non_joker_values:
                new_rank = rank(remaining_cards + [joker])
                if new_rank.value < self.rank.value:
                    self.rank = new_rank

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

    def test_rerank_with_joker_four_to_five(self):
        card = Card('KKKJK', '1')
        card.rerank_with_joker()
        self.assertEqual(card.rank, CardType.FIVE)

    def test_rerank_with_joker_high_to_pair(self):
        card = Card('23KTJ', '1')
        card.rerank_with_joker()
        self.assertEqual(card.rank, CardType.PAIR)

if __name__ == '__main__':
    unittest.main(argv=sys.argv[:1], exit=False)
    print()
    # exit(1)

    cards = parse_cards()

    res1 = get_winnings(cards)
    assert res1 == (6440 if is_sample else 248179786)
    print(f"Part 1: {res1}{' (sample)' if is_sample else ''}")

    for card in cards:
        card.rerank_with_joker()

    res2 = get_winnings(cards)
    assert res2 == (5905 if is_sample else 247885995)
    print(f"Part 2: {res2}{' (sample)' if is_sample else ''}")
