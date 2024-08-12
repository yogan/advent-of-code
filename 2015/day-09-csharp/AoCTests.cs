using Xunit;

public class AoCTests
{
    static readonly string[] Sample = new [] {
        "London to Dublin = 464",
        "London to Belfast = 518",
        "Dublin to Belfast = 141",
    };


    [Fact]
    public void ParseInput_WorksForSample() {
        var graph = AoC.ParseInput(Sample);

        Assert.Equal(new [] {"London", "Dublin", "Belfast"},
                     graph.Nodes.ToArray());

        Assert.Equal(new [] {
            new Edge("London",  "Dublin",  464),
            new Edge("Dublin",  "London",  464),
            new Edge("London",  "Belfast", 518),
            new Edge("Belfast", "London",  518),
            new Edge("Dublin",  "Belfast", 141),
            new Edge("Belfast", "Dublin",  141),
        }, graph.Edges.ToArray());
    }

    [Fact]
    public void Part1_WorksForSample() {
        Assert.Equal(605, AoC.Part1(AoC.ParseInput(Sample)));
    }
}
