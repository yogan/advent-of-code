local aoc = require("aoc")

local sample = {
	"root@ebhq-gridcenter# df -h",
	"Filesystem            Size  Used  Avail  Use%",
	"/dev/grid/node-x0-y0   10T    8T     2T   80%",
	"/dev/grid/node-x0-y1   11T    6T     5T   54%",
	"/dev/grid/node-x0-y2   32T   28T     4T   87%",
	"/dev/grid/node-x1-y0    9T    7T     2T   77%",
	"/dev/grid/node-x1-y1    8T    0T     8T    0%",
	"/dev/grid/node-x1-y2   11T    7T     4T   63%",
	"/dev/grid/node-x2-y0   10T    6T     4T   60%",
	"/dev/grid/node-x2-y1    9T    8T     1T   88%",
	"/dev/grid/node-x2-y2    9T    6T     3T   66%",
}

describe("parse", function()
	it("should parse the part 2 sample df output", function()
		assert.are.same({
			max_x = 2,
			max_y = 2,
			nodes = {
				[0] = {
					[0] = { size = 10, used = 8, avail = 2 },
					[1] = { size = 11, used = 6, avail = 5 },
					[2] = { size = 32, used = 28, avail = 4 },
				},
				[1] = {
					[0] = { size = 9, used = 7, avail = 2 },
					[1] = { size = 8, used = 0, avail = 8 },
					[2] = { size = 11, used = 7, avail = 4 },
				},
				[2] = {
					[0] = { size = 10, used = 6, avail = 4 },
					[1] = { size = 9, used = 8, avail = 1 },
					[2] = { size = 9, used = 6, avail = 3 },
				},
			},
		}, aoc.parse(sample))
	end)
end)

describe("part 1", function()
	it("should count the viable pairs for the sample", function()
		assert.are.equal(7, aoc.part1(aoc.parse(sample)))
	end)
end)
