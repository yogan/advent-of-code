using System.Text.RegularExpressions;

namespace aoc;

public interface Instruction {}
record struct Do()              : Instruction {}
record struct Dont()            : Instruction {}
record struct Mul(int X, int Y) : Instruction {}

public static class AoC
{
    public static IEnumerable<Instruction> ParseInput(IEnumerable<string> lines) =>
        new Regex(@"mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)")
            .Matches(string.Join("", lines))
            .Select<Match, Instruction>(match =>
            {
                switch (match.Value)
                {
                    case "do()":
                        return new Do();
                    case "don't()":
                        return new Dont();
                    default:
                        return new Mul(int.Parse(match.Groups[1].Value),
                                       int.Parse(match.Groups[2].Value));
                }
            });

    public static int Part1(IEnumerable<Instruction> instructions) =>
        instructions.Sum(i => i switch { Mul m => m.X * m.Y, _ => 0 });

    public static int Part2(IEnumerable<Instruction> instructions) =>
        instructions.Aggregate((0, 1), (acc, i) => i switch
        {
            Do _   => (acc.Item1, 1),
            Dont _ => (acc.Item1, 0),
            Mul m  => (acc.Item1 + m.X * m.Y * acc.Item2, acc.Item2),
            _      =>  acc
        }).Item1;
}
