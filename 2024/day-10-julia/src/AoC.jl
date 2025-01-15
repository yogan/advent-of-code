module AoC

parseInput(lines) = [[parse(Int, c) for c in line] for line in lines]

trailheads(grid) = [(r, c) for (r, row) in enumerate(grid)
                    for (c, v) in enumerate(row) if v == 0]

neighbors((r, c), dim) =
    Set([(r + dr, c + dc) for (dr, dc) in [(0, 1), (1, 0), (0, -1), (-1, 0)]
         if 0 < r + dr <= dim && 0 < c + dc <= dim])

function score_and_rating(grid, trailhead)
    dim = size(grid, 1)
    paths = [[trailhead]]

    for height in 0:8
        next_paths = []
        for path in paths
            for (r, c) in neighbors(path[end], dim)
                if grid[r][c] == height + 1
                    push!(next_paths, push!(copy(path), (r, c)))
                end
            end
            paths = next_paths
        end
    end

    peaks = Set(path[end] for path in paths)

    return length(peaks), length(paths)
end

export parseInput, trailheads, neighbors, score_and_rating

end
