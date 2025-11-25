import sys
import unittest

NORTH, EAST, SOUTH, WEST = (1, 0), (0, 1), (-1, 0), (0, -1)


def parse(filename):
    return [(i[0], int(i[1:])) for i in open(filename).read().strip().split(",")]


def flood_fill(instructions):
    walls, end = build_maze(instructions)
    steps = 0
    seen = set()
    border = set([(0, 0)])

    while True:
        if end in border:
            # print_maze(walls, border, seen, steps, end)
            return steps

        seen |= border
        steps += 1

        border = set(
            (nr, nc)
            for r, c in border
            for nr, nc in neighbors(r, c, walls)
            if (nr, nc) not in seen
        )


def part3(instructions):
    return 0


def build_maze(instructions):
    r, c = 0, 0
    dr, dc = NORTH
    walls = set()

    for turn, dist in instructions:
        dr, dc = direction((dr, dc), turn)
        for _ in range(dist):
            r += dr
            c += dc
            walls.add((r, c))

    end = (r, c)
    walls.remove(end)
    return walls, end


def neighbors(r, c, walls):
    return {(r - 1, c), (r, c - 1), (r, c + 1), (r + 1, c)} - walls


def direction(cur, turn):
    if cur == NORTH:
        return EAST if turn == "R" else WEST
    if cur == EAST:
        return SOUTH if turn == "R" else NORTH
    if cur == SOUTH:
        return WEST if turn == "R" else EAST
    if cur == WEST:
        return NORTH if turn == "R" else SOUTH
    raise (ValueError(f"direction({cur=}, {turn=})"))


def print_maze(walls, border, seen, steps, end):
    min_r = min(r for r, _ in walls)
    min_c = min(c for _, c in walls)
    max_r = max(r for r, _ in walls)
    max_c = max(c for _, c in walls)

    print(f"\n{steps=}")
    for r in range(max_r, min_r - 1, -1):
        for c in range(min_c, max_c + 1):
            if (r, c) == (0, 0):
                print("S", end="")
            elif (r, c) == end:
                print("E", end="")
            elif (r, c) in walls:
                print("#", end="")
            elif (r, c) in border:
                print("x", end="")
            elif (r, c) in seen:
                print(".", end="")
            else:
                print(" ", end="")
        print()


def generate_svg(instructions, filename="maze.svg"):
    """Generate an SVG visualization showing path as straight line segments."""

    # First pass: calculate all corner points to determine bounds
    r, c = 0, 0
    dr, dc = NORTH
    corner_points = [(0, 0)]  # Start point

    for turn, dist in instructions:
        dr, dc = direction((dr, dc), turn)
        r += dr * dist  # Move the full distance in one step
        c += dc * dist
        corner_points.append((r, c))

    # Calculate bounds from corner points only
    min_r = min(r for r, c in corner_points)
    min_c = min(c for r, c in corner_points)
    max_r = max(r for r, c in corner_points)
    max_c = max(c for r, c in corner_points)

    # SVG parameters - scale for square maze, larger and zoomable
    target_size = 5000  # Target size for square maze
    margin = 40

    coord_width = max_c - min_c
    coord_height = max_r - min_r
    coord_max = max(coord_width, coord_height)

    # Calculate scale based on the larger dimension
    scale = (target_size - 2 * margin) / coord_max if coord_max > 0 else 1

    # Final dimensions (square)
    width = coord_width * scale + 2 * margin
    height = coord_height * scale + 2 * margin

    # Convert coordinate system (SVG y increases downward)
    def to_svg_coords(r, c):
        svg_x = (c - min_c) * scale + margin
        svg_y = (max_r - r) * scale + margin
        return svg_x, svg_y

    # Start SVG
    svg_lines = [
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" height="{height}" viewBox="0 0 {width} {height}">',
        f'<rect width="{width}" height="{height}" fill="white"/>',
    ]

    # Draw lines between corner points with length labels
    for i, (turn, dist) in enumerate(instructions):
        start_r, start_c = corner_points[i]
        end_r, end_c = corner_points[i + 1]

        x1, y1 = to_svg_coords(start_r, start_c)
        x2, y2 = to_svg_coords(end_r, end_c)
        svg_lines.append(
            f'<line x1="{x1}" y1="{y1}" x2="{x2}" y2="{y2}" stroke="black" stroke-width="1"/>'
        )

        # Add length label at the middle of the line, offset from the line
        mid_x = (x1 + x2) / 2
        mid_y = (y1 + y2) / 2

        # Calculate perpendicular offset to move text away from line
        line_dx = x2 - x1
        line_dy = y2 - y1
        line_length = (line_dx**2 + line_dy**2) ** 0.5

        if line_length > 0:
            # Perpendicular vector (rotate by 90 degrees)
            perp_x = -line_dy / line_length
            perp_y = line_dx / line_length

            # Offset distance
            offset = 15
            text_x = mid_x + perp_x * offset
            text_y = mid_y + perp_y * offset
        else:
            text_x = mid_x
            text_y = mid_y

        # Calculate font size based on scale (smaller for larger mazes)
        font_size = max(8, min(16, scale / 10))

        svg_lines.append(
            f'<text x="{text_x}" y="{text_y}" font-family="Arial" font-size="{font_size}" '
            f'fill="red" text-anchor="middle" dominant-baseline="middle">{dist}</text>'
        )

    # Mark start point
    start_x, start_y = to_svg_coords(0, 0)
    svg_lines.append(f'<circle cx="{start_x}" cy="{start_y}" r="5" fill="green"/>')
    svg_lines.append(
        f'<text x="{start_x + 15}" y="{start_y - 10}" font-family="Arial" font-size="12" fill="green">Start</text>'
    )

    # Mark end point (last corner point)
    end_r, end_c = corner_points[-1]
    end_x, end_y = to_svg_coords(end_r, end_c)
    svg_lines.append(f'<circle cx="{end_x}" cy="{end_y}" r="5" fill="red"/>')
    svg_lines.append(
        f'<text x="{end_x + 15}" y="{end_y - 10}" font-family="Arial" font-size="12" fill="red">End</text>'
    )

    svg_lines.append("</svg>")

    # Write to file
    with open(filename, "w") as f:
        f.write("\n".join(svg_lines))

    print(f"SVG saved to {filename}")


def generate_compressed_svg(instructions, filename="compressed_maze.svg"):
    """Generate an SVG showing the coordinate-compressed maze as a grid."""

    # Step 1: Extract all wall segments (as line segments, not individual points)
    wall_segments = []
    r, c = 0, 0
    dr, dc = NORTH

    for turn, dist in instructions:
        dr, dc = direction((dr, dc), turn)
        start_r, start_c = r, c
        r += dr * dist
        c += dc * dist
        end_r, end_c = r, c

        # Store wall segment
        wall_segments.append(((start_r, start_c), (end_r, end_c)))

    # Step 2: Extract critical coordinates (wall boundaries + gaps)
    critical_x = set()
    critical_y = set()

    # Add start and end coordinates
    critical_x.add(0)
    critical_y.add(0)
    critical_x.add(r)
    critical_y.add(c)

    # Add all wall segment endpoints and adjacent coordinates
    for (start_r, start_c), (end_r, end_c) in wall_segments:
        # Add wall endpoints
        critical_x.update([start_c, end_c])
        critical_y.update([start_r, end_r])

        # Add coordinates adjacent to walls (to create gaps)
        if start_c == end_c:  # Vertical wall
            critical_x.update([start_c - 1, start_c + 1])
        if start_r == end_r:  # Horizontal wall
            critical_y.update([start_r - 1, start_r + 1])

    # Step 3: Create sorted coordinate mappings
    x_coords = sorted(critical_x)
    y_coords = sorted(critical_y)

    print(
        f"Compression: {len(x_coords)}×{len(y_coords)} grid (was ~{max(x_coords) - min(x_coords)}×{max(y_coords) - min(y_coords)})"
    )

    # Create mapping from real coords to compressed coords
    x_to_compressed = {x: i for i, x in enumerate(x_coords)}
    y_to_compressed = {y: i for i, y in enumerate(y_coords)}

    # Step 4: Build compressed grid - mark wall cells
    grid_width = len(x_coords)
    grid_height = len(y_coords)
    walls_compressed = set()

    for (start_r, start_c), (end_r, end_c) in wall_segments:
        # Mark all cells along this wall segment in compressed coordinates
        comp_start_x = x_to_compressed[start_c]
        comp_start_y = y_to_compressed[start_r]
        comp_end_x = x_to_compressed[end_c]
        comp_end_y = y_to_compressed[end_r]

        # Fill in all cells along the line segment
        if start_c == end_c:  # Vertical wall
            min_y = min(comp_start_y, comp_end_y)
            max_y = max(comp_start_y, comp_end_y)
            for comp_y in range(min_y, max_y + 1):
                walls_compressed.add((comp_y, comp_start_x))
        elif start_r == end_r:  # Horizontal wall
            min_x = min(comp_start_x, comp_end_x)
            max_x = max(comp_start_x, comp_end_x)
            for comp_x in range(min_x, max_x + 1):
                walls_compressed.add((comp_start_y, comp_x))

    # Step 5: Generate SVG
    cell_size = max(
        2, min(20, 800 // max(grid_width, grid_height))
    )  # Auto-scale cell size
    margin = 20
    width = grid_width * cell_size + 2 * margin
    height = grid_height * cell_size + 2 * margin

    svg_lines = [
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" height="{height}" viewBox="0 0 {width} {height}">',
        f'<rect width="{width}" height="{height}" fill="white"/>',
    ]

    # Draw grid cells
    for comp_y in range(grid_height):
        for comp_x in range(grid_width):
            x = comp_x * cell_size + margin
            y = (grid_height - 1 - comp_y) * cell_size + margin  # Flip Y axis

            if (comp_y, comp_x) in walls_compressed:
                # Wall cell - black
                svg_lines.append(
                    f'<rect x="{x}" y="{y}" width="{cell_size}" height="{cell_size}" fill="black"/>'
                )
            else:
                # Free space - light gray with border
                svg_lines.append(
                    f'<rect x="{x}" y="{y}" width="{cell_size}" height="{cell_size}" fill="lightgray" stroke="gray" stroke-width="0.1"/>'
                )

    # Mark start position
    start_comp_x = x_to_compressed[0]
    start_comp_y = y_to_compressed[0]
    start_x = start_comp_x * cell_size + margin + cell_size // 2
    start_y = (grid_height - 1 - start_comp_y) * cell_size + margin + cell_size // 2
    svg_lines.append(
        f'<circle cx="{start_x}" cy="{start_y}" r="{cell_size // 3}" fill="green"/>'
    )

    # Mark end position
    end_comp_x = x_to_compressed[r]
    end_comp_y = y_to_compressed[c]
    end_x = end_comp_x * cell_size + margin + cell_size // 2
    end_y = (grid_height - 1 - end_comp_y) * cell_size + margin + cell_size // 2
    svg_lines.append(
        f'<circle cx="{end_x}" cy="{end_y}" r="{cell_size // 3}" fill="red"/>'
    )

    svg_lines.append("</svg>")

    # Write to file
    with open(filename, "w") as f:
        f.write("\n".join(svg_lines))

    print(f"Compressed SVG saved to {filename}")


class Tests(unittest.TestCase):
    def test_build_maze(self):
        instr = [("R", 3), ("R", 4), ("L", 3), ("L", 4), ("R", 3), ("R", 6), ("R", 9)]

        w1 = [(0, 1), (0, 2), (0, 3)]
        w2 = w1 + [(-1, 3), (-2, 3), (-3, 3), (-4, 3)]
        w3 = w2 + [(-4, 4), (-4, 5), (-4, 6)]
        w4 = w3 + [(-3, 6), (-2, 6), (-1, 6), (0, 6)]
        w5 = w4 + [(0, 7), (0, 8), (0, 9)]
        w6 = w5 + [(-1, 9), (-2, 9), (-3, 9), (-4, 9), (-5, 9), (-6, 9)]
        w7 = w6 + [(-6, c) for c in range(8, -1, -1)]

        self.assertEqual(build_maze(instr[:1]), (set(w1[:-1]), w1[-1]))
        self.assertEqual(build_maze(instr[:2]), (set(w2[:-1]), w2[-1]))
        self.assertEqual(build_maze(instr[:3]), (set(w3[:-1]), w3[-1]))
        self.assertEqual(build_maze(instr[:4]), (set(w4[:-1]), w4[-1]))
        self.assertEqual(build_maze(instr[:5]), (set(w5[:-1]), w5[-1]))
        self.assertEqual(build_maze(instr[:6]), (set(w6[:-1]), w6[-1]))
        self.assertEqual(build_maze(instr[:7]), (set(w7[:-1]), w7[-1]))


def main():
    failures = 0

    if create_svgs:
        generate_svg(parse("input3.txt"))
        generate_compressed_svg(parse("input3.txt"))
    elif is_sample:
        failures += check(1, flood_fill(parse("sample1.txt")), 16)
    else:
        failures += check(1, flood_fill(parse("input1.txt")), 101)
        failures += check(2, flood_fill(parse("input2.txt")), 4296)
        failures += check(3, part3(parse("input3.txt")))

    exit(failures)


def check(part, actual, expected=None):
    failure = 0

    if expected is None:
        symbol = "🤔"
        result = f"{actual}"
    elif actual == expected:
        symbol = "✅"
        result = f"{actual}"
    else:
        symbol = "❌"
        result = f"{actual} ≠ {expected}"
        failure = 1

    print(f"{symbol} Part {part}{' (sample)' if is_sample else ''}: {result}")
    return failure


if __name__ == "__main__":
    flags = set(arg for arg in sys.argv[1:] if arg.startswith("-"))

    is_sample = "-s" in flags or "--sample" in flags
    run_tests = "-t" in flags or "--test" in flags
    create_svgs = "-c" in flags or "--create-svgs" in flags

    if run_tests:
        unittest.main(argv=sys.argv[:1])

    main()
