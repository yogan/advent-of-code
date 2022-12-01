import re
from input import read_and_solve
from functools import reduce


def parse_drawn_numbers(line):
    return list(map(int, line.split(",")))


def parse_boards(lines):
    boards = []
    for i in range(len(lines)):
        if lines[i].isspace():
            board = []
            continue
        splits = re.split(r'[ ]+', lines[i].strip())
        board.append(list(map(lambda s: (int(s), False), splits)))

        if (i+1) % 6 == 0:
            boards.append(board)

    return boards


def mark_matches(board, number):
    for i in range(len(board)):
        for j in range(len(board[i])):
            if board[i][j][0] == number:
                board[i][j] = (number, True)
    return


def board_is_solved(board):
    for row in board:
        solved = reduce(lambda a, b: a & b, map(lambda t: t[1], row))
        if solved:
            return True

    for i in range(len(board[0])):
        column = list(map(lambda row: row[i][1], board))
        solved = reduce(lambda a, b: a & b, column)
        if solved:
            return True

    return False


def sum_unmarked(board):
    sum = 0
    for row in board:
        for entry in row:
            if entry[1] == False:
                sum += entry[0]

    return sum


def part1(lines):
    drawn_numbers = parse_drawn_numbers(lines[0])
    boards = parse_boards(lines[1::])

    for number in drawn_numbers:
        for board in boards:
            mark_matches(board, number)
            if board_is_solved(board):
                sum = sum_unmarked(board)
                return sum * number

    raise Exception("No board solved.")


def part2(lines):
    drawn_numbers = parse_drawn_numbers(lines[0])
    boards = parse_boards(lines[1::])

    for number in drawn_numbers:
        remaining_boards = []

        for i in range(len(boards)):
            board = boards[i]
            mark_matches(board, number)

            if board_is_solved(board):
                if len(boards) == 1:
                    sum = sum_unmarked(board)
                    return sum * number
            else:
                remaining_boards.append(board)

        boards = remaining_boards

    raise Exception("Not all boards solved.")


def test_sum_unmarked():
    input_board = [
        [(22, True), (13, True), (17, False), (11, True), (0, True)],
        [(8, True), (2, False), (23, False), (4, True), (24, True)],
        [(21, True), (9, False), (14, True), (16, True), (7, False)],
        [(6, True), (10, True), (3, False), (18, True), (5, False)],
        [(1, True), (12, False), (20, True), (15, False), (19, True)]]

    assert sum_unmarked(input_board) == 93


def test_board_is_solved():
    input_board_column_solved = [
        [(22, True), (13, False), (17, False), (11, False), (0, False)],
        [(8, True), (2, False), (23, False), (4, False), (24, False)],
        [(21, True), (9, False), (14, False), (16, False), (7, False)],
        [(6, True), (10, False), (3, False), (18, False), (5, False)],
        [(1, True), (12, False), (20, False), (15, False), (19, False)]]

    input_board_row_solved = [
        [(22, True), (13, True), (17, True), (11, True), (0, True)],
        [(8, False), (2, False), (23, False), (4, False), (24, False)],
        [(21, False), (9, False), (14, False), (16, False), (7, False)],
        [(6, False), (10, False), (3, False), (18, False), (5, False)],
        [(1, False), (12, False), (20, False), (15, False), (19, False)]]

    input_board_not_solved = [
        [(22, True), (13, False), (17, True), (11, True), (0, True)],
        [(8, False), (2, False), (23, False), (4, False), (24, False)],
        [(21, False), (9, False), (14, False), (16, False), (7, False)],
        [(6, False), (10, False), (3, False), (18, False), (5, False)],
        [(1, False), (12, False), (20, False), (15, False), (19, False)]]

    assert board_is_solved(input_board_row_solved) == True
    assert board_is_solved(input_board_column_solved) == True
    assert board_is_solved(input_board_not_solved) == False


def test_mark_matches():
    input_board = [
        [(22, False), (13, False), (17, False), (11, False), (0, False)],
        [(8, False), (2, False), (23, False), (4, False), (24, False)],
        [(21, False), (9, False), (14, False), (16, False), (7, False)],
        [(6, False), (10, False), (3, False), (18, False), (5, False)],
        [(1, False), (12, False), (20, False), (15, False), (19, False)]]
    input_number = 2
    # print(input_board[1][1])
    mark_matches(input_board, input_number)
    # print(input_board[1][1])
    assert input_board[1][1] == (2, True)


def test_parse_draw_numbers():
    input = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
    result = parse_drawn_numbers(input)
    assert result == [7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24,
                      10, 16, 13, 6, 15, 25, 12, 22, 18, 20, 8, 19, 3, 26, 1]


def test_parse_boards():
    input = [
        "\n",
        "22 13 17 11  0",
        " 8  2 23  4 24",
        "21  9 14 16  7",
        " 6 10  3 18  5",
        " 1 12 20 15 19",
        "\n",
        " 3 15  0  2 22",
        " 9 18 13 17  5",
        "19  8  7 25 23",
        "20 11 10 24  4",
        "14 21 16 12  6",
        "\n",
        "14 21 17 24  4",
        "10 16 15  9 19",
        "18  8 23 26 20",
        "22 11 13  6  5",
        " 2  0 12  3  7"
    ]

    result = parse_boards(input)
    # print(result)
    assert len(result) == 3
    assert len(result[0]) == 5
    assert len(result[0][0]) == 5
    assert result[0][0][0] == (22, False)
    assert result[0][0][4] == (0, False)
    assert result[1][1][1] == (18, False)
    assert result[2][4][4] == (7, False)


test_parse_draw_numbers()
test_parse_boards()
test_mark_matches()
test_board_is_solved()
test_sum_unmarked()

read_and_solve(__file__, part1, part2)
