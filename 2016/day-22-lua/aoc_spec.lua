local aoc = require("aoc")

describe("parse", function()
	it("should parse df output", function()
		local lines = {
			"root@ebhq-gridcenter# df -h",
			"Filesystem              Size  Used  Avail  Use%",
			"/dev/grid/node-x0-y0     88T   67T    21T   76%",
			"/dev/grid/node-x0-y1     85T   73T    12T   85%",
			"/dev/grid/node-x0-y2     94T   73T    21T   77%",
		}

		assert.are.same({
			{ x = 0, y = 0, size = 88, used = 67, avail = 21, use = 76 },
			{ x = 0, y = 1, size = 85, used = 73, avail = 12, use = 85 },
			{ x = 0, y = 2, size = 94, used = 73, avail = 21, use = 77 },
		}, aoc.parse(lines))
	end)
end)
