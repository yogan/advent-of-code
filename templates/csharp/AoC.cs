public record struct Box(int Length, int Width, int Height) {
    public int Area => 2 * (Length * Width + Width * Height + Height * Length);
    public int Volume => Length * Width * Height;
}

public class AoC
{
    public static IEnumerable<Box> ParseInput(IEnumerable<string> lines) =>
        lines.Select(line => {
            var parts = line.Split('x').Select(int.Parse).ToArray();
            return new Box(parts[0], parts[1], parts[2]);
        });

    public static int Part1(IEnumerable<Box> boxes) =>
        boxes.Sum(box => box.Volume);

    public static int Part2(IEnumerable<Box> boxes) =>
        boxes.Sum(box => box.Area);
}
