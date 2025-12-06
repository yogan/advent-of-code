module AoC
export part1, part2

function part1(lines)
    data = split.(strip.(lines))
    ops, rows = data[end], rotate(data[1:end-1])
    sum(calc(op, nums) for (op, nums) in zip(ops, rows))
end

function part2(lines)
    total, op, nums = 0, "", String[]

    for row in rotate(lines)
        new_op, digits = row[1], row[2:end]
        if isempty(strip(join(digits)))
            total += calc(op, nums)
            nums = String[]
        else
            new_op != ' ' && (op = string(new_op))
            push!(nums, join(reverse(digits)))
        end
    end

    total
end

rotate(data) = eachrow(rotr90(stack(data, dims=1)))

calc(op, strings) = strings |>
                    (str -> parse.(Int, str)) |>
                    (nums -> op == "+" ? sum(nums) : prod(nums))

end
