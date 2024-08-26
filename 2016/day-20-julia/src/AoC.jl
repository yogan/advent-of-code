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

function countUnblocked(blocked::Set{Range})::UInt32
    count, cur, next = 0, 0, 0

    while !isempty(blocked)
        matching = filter(r -> r.from <= cur <= r.to, blocked)

        if (isempty(matching))
            next = minimum([r.from for r in blocked])
            count += next - cur
            cur = next
        else
            cur = maximum([r.to for r in matching]) + 1
            setdiff!(blocked, filter(r -> r.to < cur, blocked))
        end
    end

    return count + 4294967295 - cur + 1
end

part1(blocked::Array{Range})::UInt32 = lowestUnblocked(Set(blocked))
part2(blocked::Array{Range})::UInt32 = countUnblocked(Set(blocked))

export Range, parseInput, lowestUnblocked, countUnblocked, part1, part2

end
