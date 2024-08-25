module AoC

parseInput(line) = [parse(Int, c) for c in line]

function solve(digits, offset)
    match(i) = digits[i] == digits[mod1(i + offset, length(digits))]
    return sum([digits[i] for i in eachindex(digits) if match(i)])
end

part1(digits) = solve(digits, 1)
part2(digits) = solve(digits, length(digits) ÷ 2)

export parseInput, part1, part2

end
