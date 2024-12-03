using Xunit;

namespace aoc;

public class AoCTests
{
    static readonly string[] Samples = [
        "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))",
        "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
    ];

    [Fact]
    public void ParseInput_WorksForSamples() =>
        Assert.Equal([
            new Mul(2, 4),
            new Mul(5, 5),
            new Mul(11, 8),
            new Mul(8, 5),

            new Mul(2, 4),
            new Dont(),
            new Mul(5, 5),
            new Mul(11, 8),
            new Do(),
            new Mul(8, 5)
        ], AoC.ParseInput(Samples));

    [Fact]
    public void Part1_WorksForFirstSample() =>
        Assert.Equal(161, AoC.Part1(AoC.ParseInput([Samples.First()])));

    [Fact]
    public void Part2_WorksForSecondSample() =>
        Assert.Equal(48, AoC.Part2(AoC.ParseInput([Samples.Last()])));
}
