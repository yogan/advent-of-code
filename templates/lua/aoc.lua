local M = {}

table.unpack = table.unpack or unpack -- 5.1 compatibility

local function surface_area(box)
	local l, w, h = table.unpack(box)
	local lw = l * w
	local wh = w * h
	local hl = h * l
	return 2 * lw + 2 * wh + 2 * hl
end

local function volume(box)
	local l, w, h = table.unpack(box)
	return l * w * h
end

function M.parse(lines)
	local dimensions = {}
	for _, line in ipairs(lines) do
		local dimension = {}
		for value in line:gmatch("%d+") do
			table.insert(dimension, tonumber(value))
		end
		table.insert(dimensions, dimension)
	end
	return dimensions
end

function M.part1(boxes)
	local res = 0
	for _, box in ipairs(boxes) do
		res = res + volume(box)
	end
	return res
end

function M.part2(boxes)
	local res = 0
	for _, box in ipairs(boxes) do
		res = res + surface_area(box)
	end
	return res
end

return M
