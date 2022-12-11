using Xunit;

public class Day11Tests
{
    private const string Filename = "day11.sample";

    [Fact]
    public void Part1_WithSample_ReturnsMonkeyBusiness()
    {
        var monkeyBusiness = Day11.Part1(Filename);

        Assert.Equal(10605, monkeyBusiness);
    }

    [Fact]
    public void Part2_WithSample_ReturnsMonkeyBusiness()
    {
        var monkeyBusiness = Day11.Part2(Filename);

        Assert.Equal(2713310158, monkeyBusiness);
    }

    [Fact]
    public void SimulateMonkeyBusiness_WithSample_Part1_CountsInspections()
    {
        var monkeys = Day11.ParseInput(Filename);

        var inspections = Day11.SimulateMonkeyBusiness(monkeys, 20);

        Assert.Equal(new Int64[] { 101, 95, 7, 105 }, inspections);
    }

    [Fact]
    public void SimulateMonkeyBusiness_WithSample_Part2_CountsInspections()
    {
        var monkeys = Day11.ParseInput(Filename);

        var inspections = Day11.SimulateMonkeyBusiness(monkeys, 10000);

        Assert.Equal(new Int64[] { 52166, 47830, 1938, 52013 }, inspections);
    }

    [Fact]
    public void ParseInput_WithSampleFile_ReturnsMonkeys()
    {
        var monkeys = Day11.ParseInput(Filename);

        Assert.Equal(new[]
        {
            new Stack<Int64>(new Int64[] { 79, 98 }),
            new Stack<Int64>(new Int64[] { 54, 65, 75, 74 }),
            new Stack<Int64>(new Int64[] { 79, 60, 97 }),
            new Stack<Int64>(new Int64[] { 74 }),
        }, monkeys.Select(monkey => monkey.Items));

        Assert.Equal(new Operation[] {
            new Multiply(19), new Add(6), new Square(), new Add(3),
        }, monkeys.Select(monkey => monkey.Operation));

        Assert.Equal(new[] { 23, 19, 13, 17
        }, monkeys.Select(monkey => monkey.Divisor));

        Assert.Equal(new[] { 2, 2, 1, 0
        }, monkeys.Select(monkey => monkey.TargetTrue));

        Assert.Equal(new[] { 3, 0, 3, 1
        }, monkeys.Select(monkey => monkey.TargetFalse));
    }
}