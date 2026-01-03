using Xunit;

public class AoCTests
{
    static readonly string[] Sample = ["1x2x3", "987x10x1"];

    [Fact]
    public void ParseInput_WorksForSample() =>
        Assert.Equal([new Box(1, 2, 3), new Box(987, 10, 1)],
                     AoC.ParseInput(Sample));

    [Fact]
    public void Part1_ReturnsTheSumOfTheVolumes() =>
        Assert.Equal(6 + 9870, AoC.Part1(AoC.ParseInput(Sample)));
}