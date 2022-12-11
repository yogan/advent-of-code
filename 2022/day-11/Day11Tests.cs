using Xunit;

public class Day11Tests
{
    private const string Filename = "day11.sample";

    [Fact]
    public void ParseInput_WithSampleFile_ReturnsMonkeys()
    {
        var monkeys = Day11.ParseInput(Filename);

        Assert.Equal(new[]
        {
            new Stack<int>(new int[] { 79, 98 }),
            new Stack<int>(new int[] { 54, 65, 75, 74 }),
            new Stack<int>(new int[] { 79, 60, 97 }),
            new Stack<int>(new int[] { 74 }),
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

    [Fact]
    public void SimulateMonkeyBusiness_WithSample_CountsInspections()
    {
        var monkeys = Day11.ParseInput(Filename);

        var inspections = Day11.SimulateMonkeyBusiness(monkeys, 20);

        Assert.Equal(new[] { 101, 95, 7, 105 }, inspections);
    }

    [Fact]
    public void Part1_WithSample_ReturnsMonkeyBusiness()
    {
        var monkeyBusiness = Day11.Part1(Filename);

        Assert.Equal(10605, monkeyBusiness);
    }
}