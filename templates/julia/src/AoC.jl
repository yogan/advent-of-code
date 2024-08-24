module AoC

function parseInput(lines)
    lines = [split(line, "x") for line in lines]
    return map(line -> Tuple(map(num -> parse(Int, num), line)), lines)
end

volume(l, w, h) = l * w * h
surfaceArea(l, w, h) = 2 * (l * w + w * h + h * l)

part1(boxes) = sum([volume(l, w, h) for (l, w, h) in boxes])
part2(boxes) = sum([surfaceArea(l, w, h) for (l, w, h) in boxes])

export parseInput, part1, part2

end
