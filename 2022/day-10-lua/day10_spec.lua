local day10 = require("day10")

describe("Advent of Code 2022 Day 10", function()
	describe("parse_input", function()
		it("should calculate the signal strengths for the sample input", function()
			local result = day10.parse_input("day10.sample").signal_strengths
			assert.are.same({ 420, 1140, 1800, 2940, 2880, 3960 }, result)
		end)

		it("should return the signal strength sum the sample input", function()
			local result = day10.parse_input("day10.sample")
			assert.are.same(13140, result.sum)
		end)

		it("should draw the final CRT image for the sample input", function()
			local result = day10.parse_input("day10.sample")
			assert.are.same(
				table.concat({
					"##..##..##..##..##..##..##..##..##..##..",
					"###...###...###...###...###...###...###.",
					"####....####....####....####....####....",
					"#####.....#####.....#####.....#####.....",
					"######......######......######......####",
					"#######.......#######.......#######.....",
				}, "\n"),
				result.crt
			)
		end)
	end)
end)
