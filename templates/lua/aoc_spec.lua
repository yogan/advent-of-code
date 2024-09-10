local aoc = require("aoc")

describe("parse", function()
	it("should split the lines into dimensions", function()
		local result = aoc.parse({ "1x1x1", "2x3x4", "1x1x10" })
		assert.are.same({ { 1, 1, 1 }, { 2, 3, 4 }, { 1, 1, 10 } }, result)
	end)
end)

local boxes = {
	{ 1, 2, 3 },
	{ 1, 1, 1 },
}

describe("part1", function()
	it("should return the sum of the volumes", function()
		assert.are.same(6 + 1, aoc.part1(boxes))
	end)
end)

describe("part2", function()
	it("should return the sum of the surface areas", function()
		assert.are.same((2 + 2 + 3 + 3 + 6 + 6) + (6 * 1), aoc.part2(boxes))
	end)
end)
