import sys

if len(sys.argv) != 2:
    print("Missing input file.")
    exit(1)
filename  = sys.argv[1]
is_sample = filename == "sample.txt"

def parse():
    return [line.strip() for line in open(filename).readlines()]

def find_start(garden):
    R, C = len(garden), len(garden[0])
    return next((r, c) for r in range(R) for c in range(C)
                if garden[r][c] == 'S')

def travel(garden, steps=64, start=None):
    R, C = len(garden), len(garden[0])
    start = start or find_start(garden)
    plots = set([start])

    for _ in range(steps):
        next = set()
        for pos in plots:
            r, c = pos
            for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
                rr, cc = r + dr, c + dc
                if 0 <= rr < R and 0 <= cc < C and garden[rr][cc] != '#':
                    next.add((rr, cc))
        plots = next

    return len(plots)

def assert_input_structure(garden):
    """
    Asserts that the garden has the following structure:

    ```
      ┬  ░░░░░░░░░░░░░░░░░░
      │  ░░▓▓▓▓▓▓░░▓▓▓▓▓▓░░
      │  ░░▓▓▓▓▓▓░░▓▓▓▓▓▓░░     SS = start square, in the center
      │  ░░▓▓▓▓▓▓░░▓▓▓▓▓▓░░
      R  ░░░░░░░░SS░░░░░░░░     ░░ = guaranteed to have no wall
      │  ░░▓▓▓▓▓▓░░▓▓▓▓▓▓░░     ▓▓ = can have wall
      │  ░░▓▓▓▓▓▓░░▓▓▓▓▓▓░░
      │  ░░▓▓▓▓▓▓░░▓▓▓▓▓▓░░     R == C, R and C are both odd
      ┴  ░░░░░░░░░░░░░░░░░░
         ├────── C ───────┤
    ```"""

    R, C = len(garden), len(garden[0])
    assert R == C, "garden is not square"
    assert R % 2 == 1 and C % 2 == 1, "garden dimensions are not odd"

    sr, sc = find_start(garden)
    assert sr * 2 + 1 == R and sc * 2 + 1 == C, "S is not in garden center"

    assert all(garden[sr][c] != '#' for c in range(C)), "wall in start row"
    assert all(garden[r][sc] != '#' for r in range(R)), "wall in start column"

    assert all(garden[0  ][c] != '#' for c in range(C)), "wall in top row"
    assert all(garden[R-1][c] != '#' for c in range(C)), "wall in bottom row"
    assert all(garden[r][0  ] != '#' for r in range(R)), "wall in left column"
    assert all(garden[r][C-1] != '#' for r in range(R)), "wall in right column"

def diamond_count(garden, steps):
    """
    Given the structure of the input, the gardens will expand in a diamond
    shape like this (one character is a whole copy of the garden):

    ```
                 ▓▓
               ▓▓░░▓▓          SG = start garden
             ▓▓░░▓▓░░▓▓        ░░ = garden reached in even steps
           ▓▓░░▓▓░░▓▓░░▓▓      ▓▓ = garden reached in odd steps
         ▓▓░░▓▓░░SG░░▓▓░░▓▓
           ▓▓░░▓▓░░▓▓░░▓▓   ┬
             ▓▓░░▓▓░░▓▓     │  gd = garden distance, number of gardens to
               ▓▓░░▓▓       │       an edge of the diamond, here: 4
                 ▓▓         ┴
    ```

    For the inside of the diamond, we calculate:

        - the number of gardens in even steps
        - the number of gardens in odd steps
        - the plots reachable in even steps in the edge gardens
        - the plots reachable in odd steps in the edge gardens

    Those can all be multiplied and summed up, since for the inside we have more
    than enough steps to reach all odd or even positions in each garden.

    The edge has to be treated differently. Each edge garden is reached with
    steps remaining that are half the width (or height) of the garden. Those
    steps will be used to travel within the edge garden, with starting positions
    according to the position of the edge garden: top garden is entered from the
    bottom center, right from the left center, etc. The diagonal edge gardens
    are entered from the corners and appear multiple times.
    """

    size = len(garden)
    steps_to_edge = size // 2

    gd = steps // size
    assert gd % 2 == 0, "odd garden distance"  # edge gardens of diamond all odd

     #             ░░
     #           ░░▓▓░░
     #         ░░▓▓▓▓▓▓░░
     #       ░░▓▓▓▓▓▓▓▓▓▓░░
     #     ░░▓▓▓▓▓▓▓▓▓▓▓▓▓▓░░        THE INSIDE
     #       ░░▓▓▓▓▓▓▓▓▓▓░░
     #         ░░▓▓▓▓▓▓░░
     #           ░░▓▓░░
     #             ░░
    even_gardens = sum([4 * g for g in range(1, gd + 1, 2)])
    odd_gardens  = sum([4 * g for g in range(2, gd - 1, 2)]) + 1

    plots_in_odd_garden  = travel(garden, 2 * size + 1)
    plots_in_even_garden = travel(garden, 2 * size)

    plots_inside = odd_gardens  * plots_in_odd_garden \
                 + even_gardens * plots_in_even_garden

     #             ▓▓
     #           ░░░░░░
     #         ░░░░░░░░░░
     #       ░░░░░░░░░░░░░░
     #     ▓▓░░░░░░░░░░░░░░▓▓    THE CORNERS
     #       ░░░░░░░░░░░░░░
     #         ░░░░░░░░░░
     #           ░░░░░░
     #             ▓▓
    corner_steps = size - 1

    plots_top   = travel(garden, corner_steps, (size  - 1, steps_to_edge))
    plots_right = travel(garden, corner_steps, (steps_to_edge, 0))
    plots_bot   = travel(garden, corner_steps, (0, steps_to_edge))
    plots_left  = travel(garden, corner_steps, (steps_to_edge, size - 1))

    plots_corners = plots_top + plots_right + plots_bot + plots_left

     #            ▗░░▖
     #          ▗░░░░░░▖
     #        ▗░░░░░░░░░░▖
     #      ▗░░░░░░░░░░░░░░▖
     #     ░░░░░░░░░░░░░░░░░░    SMALL TRIANGLES
     #      ▝░░░░░░░░░░░░░░▘
     #        ▝░░░░░░░░░░▘
     #          ▝░░░░░░▘
     #            ▝░░▘
    small_steps = size // 2 - 1

    plots_small_top_right = travel(garden, small_steps, (size - 1, 0))
    plots_small_top_left  = travel(garden, small_steps, (size - 1, size - 1))
    plots_small_bot_right = travel(garden, small_steps, (0, 0))
    plots_small_bot_left  = travel(garden, small_steps, (0, size - 1))

    plots_small_triangles = plots_small_top_right + plots_small_top_left \
                          + plots_small_bot_right + plots_small_bot_left

     #             ░░
     #           ▄█░░█▄
     #         ▄█░░░░░░█▄
     #       ▄█░░░░░░░░░░█▄
     #     ░░░░░░░░░░░░░░░░░░    LARGE TRIANGLES
     #       ▀█░░░░░░░░░░█▀
     #         ▀█░░░░░░█▀
     #           ▀█░░█▀
     #             ░░
    large_steps = size * 3 // 2 - 1

    plots_large_top_right = travel(garden, large_steps, (size - 1, 0))
    plots_large_top_left  = travel(garden, large_steps, (size - 1, size - 1))
    plots_large_bot_right = travel(garden, large_steps, (0, 0))
    plots_large_bot_left  = travel(garden, large_steps, (0, size - 1))

    plots_large_triangles = plots_large_top_right + plots_large_top_left \
                          + plots_large_bot_right + plots_large_bot_left

    # add all the stuff together
    return plots_inside + plots_corners \
            + plots_small_triangles * gd \
            + plots_large_triangles * (gd - 1)


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
    garden = parse()
    part1 = travel(garden)
    check(1, part1, 42 if is_sample else 3816)

    if is_sample:
        # implementation is based on the specific structure of the input,
        # which the sample does not have (see assert_input_structure)
        print(f"Part 2 (sample): n/a")
        exit(0)

    assert_input_structure(garden)
    part2 = diamond_count(garden, 26501365)

    check(2, part2, 634549784009844)
