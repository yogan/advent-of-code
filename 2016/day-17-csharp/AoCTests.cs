using Xunit;

public class AoCTests
{
    [Fact]
    public void Part1And2_Sample1() =>
        Assert.Equal(("DDRRRD", 370),
                     new AoC("ihgpwlah").Part1And2());

    [Fact]
    public void Part1And2_Sample2() =>
        Assert.Equal(("DDUDRLRRUDRD", 492),
                     new AoC("kglvqrro").Part1And2());

    [Fact]
    public void Part1And2_Sample3() =>
        Assert.Equal(("DRURDRUDDLLDLUURRDULRLDUUDDDRR", 830),
                     new AoC("ulqzkmiv").Part1And2());

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
