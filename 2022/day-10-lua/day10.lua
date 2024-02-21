local day10 = {}

function replace_char(pos, str, c)
    return str:sub(1, pos - 1) .. c .. str:sub(pos + 1)
end

function string.insert(str1, str2, pos)
    return str1:sub(1, pos) .. str2 .. str1:sub(pos + 1)
end

function format(crt)
    for i = 5, 1, -1 do
        crt = string.insert(crt, "\n", 40 * i)
    end
    return crt
end

function sum(values)
    local sum = 0
    for _, value in ipairs(values) do
        sum = sum + value
    end
    return sum
end

function update_crt(crt, cycle, sprite_pos)
    local pos = cycle % (40 * 6) + 1
    local line_pos = cycle % 40 + 1
    if (line_pos == sprite_pos or line_pos == sprite_pos + 1 or line_pos == sprite_pos + 2) then
        return replace_char(pos, crt, "#")
    end
    return crt
end

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

    local crt = string.rep(".", 40 * 6)

    for line in file:lines() do
        if (line == "noop") then
            crt = update_crt(crt, cycle, x)
            cycle = cycle + 1
            if (interesting[cycle]) then
                table.insert(signalStrengths, cycle * x)
            end
        else
            crt = update_crt(crt, cycle, x)
            cycle = cycle + 1
            if (interesting[cycle]) then
                table.insert(signalStrengths, cycle * x)
            end
            crt = update_crt(crt, cycle, x)
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

    return {
        cycle = cycle,
        x = x,
        signalStrengths = signalStrengths,
        sum = sum(signalStrengths),
        crt = format(crt)
    }
end

local result = day10.parseInput("day10.in")
print("Part 1:", result.sum)
print("Part 2:")
print(result.crt)

return day10
