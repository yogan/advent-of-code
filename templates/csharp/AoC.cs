var path = Path.Combine(Environment.CurrentDirectory, args[0]);
var input = AoC.ParseInput(File.ReadAllLines(path));

Console.WriteLine(AoC.Part1(input));


public record struct Box(int Length, int Width, int Height)
{
    public Box(int[] dim) : this(dim[0], dim[1], dim[2]) { }
    public readonly int Volume => Length * Width * Height;
}

public static class AoC
{
    public static int Part1(IEnumerable<Box> boxes) => boxes.Sum(box => box.Volume);

    public static IEnumerable<Box> ParseInput(IEnumerable<string> lines) =>
        lines.Select(line => new Box(line.Split('x').Select(int.Parse).ToArray()));

}
