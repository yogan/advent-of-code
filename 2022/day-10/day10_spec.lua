local day10 = require "day10"

describe("Advent of Code 2022 Day 10", function()
    it("should parse the short sample input", function()
        local result = day10.parseInput("day10.sample.short")
        assert.are.same({
            cycle = 5,
            x = -1,
            signalStrengths = {},
            sum = 0
        }, result)
    end)

    it("should calculate the signal strengths for the sample input", function()
        local result = day10.parseInput("day10.sample")
        assert.are.same({420, 1140, 1800, 2940, 2880, 3960}, result.signalStrengths)
    end)

    it("should return the signal strength sum the sample input", function()
        local result = day10.parseInput("day10.sample")
        assert.are.same(13140, result.sum)
    end)
end)
