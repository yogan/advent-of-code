local aoc = require("aoc")

local function read_lines(filename)
	local file = io.open(filename, "r")
	if not file then
		return nil
	end
	local lines = {}
	for line in file:read("*a"):gmatch("[^\r\n]+") do
		table.insert(lines, line)
	end
	file:close()
	return lines
end

local filename = arg[1]
local lines = read_lines(filename)
local nodes = aoc.parse(lines)

print(aoc.part1(nodes))
