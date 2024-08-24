module AoC

struct Box
    l::Int
    w::Int
    h::Int
end

function parseInput(lines::Array{String})::Array{Box}
    toBox(nums)::Box = Box(nums...)
    lines = [split(line, "x") for line in lines]
    return [toBox(map(num -> parse(Int, num), line)) for line in lines]
end

volume(box::Box)::Int = box.l * box.w * box.h

function surfaceArea(box::Box)::Int
    (l, w, h) = (box.l, box.w, box.h)
    return 2 * (l * w + w * h + h * l)
end

part1(boxes)::Int = sum([volume(box) for box in boxes])
part2(boxes)::Int = sum([surfaceArea(box) for box in boxes])

export parseInput, part1, part2, Box

end
