local aoc = require("aoc")

local small_sample = {
	"AAAA",
	"BBCD",
	"BBCC",
	"EEEC",
}

local larger_sample = {
	"RRRRIICCFF",
	"RRRRIICCCF",
	"VVRRRCCFFF",
	"VVRCCCJFFF",
	"VVVVCJJCFE",
	"VVIVCCJJEE",
	"VVIIICJJEE",
	"MIIIIIJJEE",
	"MIIISIJEEE",
	"MMMISSJEEE",
}

describe("regions", function()
	it("returns area, perimeter and sides of each region", function()
		assert.are.same({
			{ 04, 10, 04 }, -- A
			{ 04, 08, 04 }, -- B
			{ 04, 10, 08 }, -- C
			{ 01, 04, 04 }, -- D
			{ 03, 08, 04 }, -- E
		}, aoc.regions(small_sample))
	end)
end)

describe("parts", function()
	it("works for the small sample", function()
		assert.are.same({ 140, 80 }, aoc.parts(aoc.regions(small_sample)))
	end)

	it("works for the larger sample", function()
		assert.are.same({ 1930, 1206 }, aoc.parts(aoc.regions(larger_sample)))
	end)
end)
