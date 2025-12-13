#!/usr/bin/env python3


def parse_shape(shape_str):
    """Convert a shape string into a set of coordinates."""
    lines = shape_str.strip().split("\n")
    coords = set()
    for r, line in enumerate(lines):
        for c, char in enumerate(line):
            if char == "#":
                coords.add((r, c))
    return coords


def normalize_shape(coords):
    """Move shape to origin (top-left)."""
    if not coords:
        return coords
    min_r = min(r for r, c in coords)
    min_c = min(c for r, c in coords)
    return {(r - min_r, c - min_c) for r, c in coords}


def rotate_90(coords):
    """Rotate shape 90 degrees clockwise."""
    return {(c, -r) for r, c in coords}


def flip_horizontal(coords):
    """Flip shape horizontally."""
    return {(r, -c) for r, c in coords}


def get_all_orientations(shape_str):
    """Get all unique orientations (rotations + reflections) of a shape."""
    coords = normalize_shape(parse_shape(shape_str))
    orientations = set()

    # Get 4 rotations
    current = coords
    for _ in range(4):
        orientations.add(frozenset(normalize_shape(current)))
        current = rotate_90(current)

    # Get 4 rotations of horizontally flipped version
    current = flip_horizontal(coords)
    for _ in range(4):
        orientations.add(frozenset(normalize_shape(current)))
        current = rotate_90(current)

    # Also try vertical flip
    current = {(-r, c) for r, c in coords}  # flip vertically
    current = normalize_shape(current)
    for _ in range(4):
        orientations.add(frozenset(normalize_shape(current)))
        current = rotate_90(current)

    return [set(orientation) for orientation in orientations]


def coords_to_string(coords):
    """Convert coordinates back to a visual string representation."""
    if not coords:
        return ""

    max_r = max(r for r, c in coords)
    max_c = max(c for r, c in coords)

    grid = [["." for _ in range(max_c + 1)] for _ in range(max_r + 1)]
    for r, c in coords:
        grid[r][c] = "#"

    return "\n".join("".join(row) for row in grid)


def get_bounding_box(coords):
    """Get the bounding box dimensions of a shape."""
    if not coords:
        return 0, 0
    max_r = max(r for r, c in coords)
    max_c = max(c for r, c in coords)
    return max_r + 1, max_c + 1


def analyze_shape_4():
    """Analyze all orientations of shape #4."""
    shape_4 = """..#
.##
##."""

    print("Original Shape #4:")
    print(shape_4)
    print()

    orientations = get_all_orientations(shape_4)
    print(f"Found {len(orientations)} unique orientations:")
    print()

    for i, orientation in enumerate(orientations):
        height, width = get_bounding_box(orientation)
        print(f"Orientation {i + 1} (dimensions: {height}x{width}):")
        print(coords_to_string(orientation))
        print()

    # Find the most compact orientations
    areas = [
        (get_bounding_box(o)[0] * get_bounding_box(o)[1], i, o)
        for i, o in enumerate(orientations)
    ]
    areas.sort()

    print("Most compact orientations:")
    for area, i, orientation in areas:
        height, width = get_bounding_box(orientation)
        print(f"Orientation {i + 1}: {height}x{width} (area: {area})")

    return orientations


def analyze_packing():
    """Analyze the given packing solution."""
    print("\nAnalyzing the given packing:")
    print("..#   1223344.")
    print(".##   11223344")
    print("##.   .112.3.4")
    print()

    # The shape appears to be in this configuration:
    # ..#
    # .##
    # ##.

    given_shape = """..#
.##
##."""

    given_coords = normalize_shape(parse_shape(given_shape))
    height, width = get_bounding_box(given_coords)

    print(f"Given packing uses dimensions: {height}x{width} (area: {height * width})")
    print("Shape pattern:")
    print(coords_to_string(given_coords))

    return given_coords


def find_more_compact_orientations():
    """Try to find more compact orientations by testing different bounding rectangles."""
    shape_4 = """..#
.##
##."""

    orientations = get_all_orientations(shape_4)

    print("Detailed analysis of all orientations:")
    print("=" * 50)

    min_area = float("inf")
    best_orientations = []

    for i, orientation in enumerate(orientations):
        height, width = get_bounding_box(orientation)
        area = height * width

        print(f"Orientation {i + 1}:")
        print(coords_to_string(orientation))
        print(f"Dimensions: {height}x{width}, Area: {area}")
        print()

        if area < min_area:
            min_area = area
            best_orientations = [(i + 1, orientation, height, width)]
        elif area == min_area:
            best_orientations.append((i + 1, orientation, height, width))

    print(f"Best orientations (minimum area {min_area}):")
    for num, orientation, h, w in best_orientations:
        print(f"Orientation {num}: {h}x{w}")
        print(coords_to_string(orientation))
        print()

    return best_orientations


if __name__ == "__main__":
    orientations = analyze_shape_4()
    given_coords = analyze_packing()

    # Check if given packing matches any orientation
    print("\nChecking if given packing matches any valid orientation...")
    given_frozen = frozenset(given_coords)

    for i, orientation in enumerate(orientations):
        if frozenset(orientation) == given_frozen:
            print(f"✅ Given packing matches orientation {i + 1}")
            break
    else:
        print("❌ Given packing does not match any valid orientation")
        print("This suggests there might be an error in the packing or interpretation.")

    print()
    best_orientations = find_more_compact_orientations()
