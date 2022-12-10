local day10 = {}

function day10.parseInput(filename)
    local file = io.open(filename, "r")
    local cycle = 0
    local x = 1
    local signalStrengths = {}

    local interesting = {
        [20] = true,
        [60] = true,
        [100] = true,
        [140] = true,
        [180] = true,
        [220] = true
    }

    for line in file:lines() do
        if (line == "noop") then
            cycle = cycle + 1
            if (interesting[cycle]) then
                table.insert(signalStrengths, cycle * x)
            end
        else
            cycle = cycle + 1
            if (interesting[cycle]) then
                table.insert(signalStrengths, cycle * x)
            end
            cycle = cycle + 1
            if (interesting[cycle]) then
                table.insert(signalStrengths, cycle * x)
            end
            for op, num in string.gmatch(line, "(addx) (.+)") do
                assert(op == "addx")
                x = x + tonumber(num)
            end
        end
    end

    local sum = 0
    for _, strength in ipairs(signalStrengths) do
        sum = sum + strength
    end

    return {
        cycle = cycle,
        x = x,
        signalStrengths = signalStrengths,
        sum = sum
    }
end

print("Part 1:", day10.parseInput("day10.in").sum)

return day10
