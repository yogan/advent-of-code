module AoC

struct Range
    from::UInt32
    to::UInt32
end

function parseInput(lines::Array{String})::Array{Range}
    toRange(from, to) = Range(parse(UInt32, from), parse(UInt32, to))
    return [toRange(split(line, "-")...) for line in lines]
end

function lowestUnblocked(blocked::Set{Range})::UInt32
    low = 0
    while true
        matching = filter(r -> r.from <= low <= r.to, blocked)
        if isempty(matching)
            return low
        end
        low = maximum([r.to for r in matching]) + 1
        setdiff!(blocked, matching)
    end
end

part1(blocked::Array{Range})::UInt32 = lowestUnblocked(Set(blocked))

export parseInput, part1, lowestUnblocked, Range

end
