module AoC
export parseInput, part1, part2, Box

struct Box
    l::Int
    w::Int
    h::Int
end

part1(boxes)::Int = sum([volume(box) for box in boxes])
part2(boxes)::Int = 0

volume(box::Box)::Int = box.l * box.w * box.h

function parseInput(lines::Array{String})::Array{Box}
    toBox(nums)::Box = Box(nums...)
    lines = [split(line, "x") for line in lines]
    [toBox(map(num -> parse(Int, num), line)) for line in lines]
end

end
