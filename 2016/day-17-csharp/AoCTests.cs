using Xunit;

public class AoCTests
{
    [Fact]
    public void Part1_Sample1() =>
        Assert.Equal("DDRRRD", new AoC("ihgpwlah").Part1());

    [Fact]
    public void Part1_Sample2() =>
        Assert.Equal("DDUDRLRRUDRD", new AoC("kglvqrro").Part1());

    [Fact]
    public void Part1_Sample3() =>
        Assert.Equal("DRURDRUDDLLDLUURRDULRLDUUDDDRR", new AoC("ulqzkmiv").Part1());

    [Fact]
    public void Hash_NoSteps() =>
        Assert.StartsWith("ced9", AoC.Hash("hijkl"));

    [Fact]
    public void Hash_OneStepDown() =>
        Assert.StartsWith("f2bc", AoC.Hash("hijklD"));

    [Fact]
    public void ToDoors_Start() =>
        Assert.Equal(new DoorStates(
                        Up:    DoorState.Open,
                        Down:  DoorState.Open,
                        Left:  DoorState.Open,
                        Right: DoorState.Closed),
                     AoC.ToDoors("ced9"));

    [Fact]
    public void ToDoors_OneStepDown() =>
        Assert.Equal(new DoorStates(
                        Up:    DoorState.Open,
                        Down:  DoorState.Closed,
                        Left:  DoorState.Open,
                        Right: DoorState.Open),
                     AoC.ToDoors("f2bc"));
}
