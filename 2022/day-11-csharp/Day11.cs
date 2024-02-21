public class Day11
{
    public const int Part1Rounds = 20;
    public const int Part2Rounds = 10000;

    private const string Filename = "day11.in";

    public static Int64 Part1(string filename = Filename) =>
        MonkeyInTheMiddle(filename, Part1Rounds);

    public static Int64 Part2(string filename = Filename) =>
        MonkeyInTheMiddle(filename, Part2Rounds);

    private static Int64 MonkeyInTheMiddle(string filename, int rounds) =>
        SimulateMonkeyBusiness(ParseInput(filename), rounds)
            .OrderByDescending(i => i)
            .Take(2)
            .Aggregate(1L, (acc, i) => acc * i);

    public static Int64[] SimulateMonkeyBusiness(IEnumerable<Monkey> monkeys, int rounds)
    {
        var inspections = new Int64[monkeys.Count()];
        var commonDivisor = monkeys.Aggregate(1, (acc, monkey) => acc * monkey.Divisor);

        for (int round = 0; round < rounds; round++)
        {
            for (int monkeyIndex = 0; monkeyIndex < monkeys.Count(); monkeyIndex++)
            {
                MonkeyTurn(monkeys, monkeyIndex, rounds, inspections, commonDivisor);
            }
        }

        return inspections;
    }

    private static void MonkeyTurn(
        IEnumerable<Monkey> monkeys, int monkeyIndex, int rounds,
        long[] inspections, int commonDivisor)
    {
        var monkey = monkeys.ElementAt(monkeyIndex);

        while (monkey.Items.TryPop(out var item))
        {
            CountInspection(inspections, monkeyIndex);
            long worryLevel = UpdateWorryLevel(monkey, item, rounds, commonDivisor);
            ThrowItem(monkeys, monkey, worryLevel);
        }
    }

    private static void CountInspection(long[] inspections, int monkeyIndex) =>
        inspections[monkeyIndex]++;

    private static long UpdateWorryLevel(Monkey monkey, long item, int rounds, int commonDivisor)
    {
        var worryLevel = monkey.Operation.Evaluate(item);
        if (rounds != Part2Rounds) { worryLevel /= 3; }
        return worryLevel % commonDivisor;
    }

    private static void ThrowItem(IEnumerable<Monkey> monkeys, Monkey monkey, long worryLevel)
    {
        var target = worryLevel % monkey.Divisor == 0
            ? monkey.TargetTrue
            : monkey.TargetFalse;
        monkeys.ElementAt(target).Items.Push(worryLevel);
    }

    public static IList<Monkey> ParseInput(string filename) =>
        File
            .ReadAllText(GetPath(filename))
            .Split("\n\n")
            .Select(group => group.Split('\n'))
            .Select(lines => new Monkey(
                Items:       ParseItems(    lines[1]),
                Operation:   ParseOperation(lines[2]),
                Divisor:     ParseDivisor(  lines[3]),
                TargetTrue:  ParseTarget(   lines[4]),
                TargetFalse: ParseTarget(   lines[5])
            ))
            .ToList();

    private static Stack<Int64> ParseItems(string line) =>
        new Stack<Int64>(
            line.Split(':').Last().Split(',').Select(Int64.Parse)
        );

    private static Operation ParseOperation(string line)
    {
        var expression = line.Split(" = ").Last();
        if (expression == "old * old")
        {
            return new Square();
        }
        var parts = expression.Split(" ");
        if (parts[1] == "+")
        {
            return new Add(int.Parse(parts[2]));
        }
        else if (parts[1] == "*")
        {
            return new Multiply(int.Parse(parts[2]));
        }
        else
        {
            throw new ArgumentException($"unsupported expression {expression}");
        }
    }

    private static int ParseDivisor(string line) =>
        int.Parse(line.Split("by").Last());

    private static int ParseTarget(string line) =>
        int.Parse(line.Split("to monkey").Last());

    private static string GetPath(string filename) =>
        // Hacky workaround for stupid dotnet cwd behavior, see:
        // https://github.com/dotnet/project-system/issues/3619
        File.Exists(filename)
            ? Path.Combine(Environment.CurrentDirectory, filename)
            : Path.Combine(Environment.CurrentDirectory, "../../../", filename);
}

public record struct Monkey(
    Stack<Int64> Items,
    Operation Operation,
    int Divisor,
    int TargetTrue,
    int TargetFalse
);

public interface Operation
{
    Int64 Evaluate(Int64 input);
}

public record struct Square : Operation
{
    public Int64 Evaluate(Int64 input) => input * input;
}

public record struct Multiply(int Value) : Operation
{
    public Int64 Evaluate(Int64 input) => Value * input;
}

public record struct Add(int Value) : Operation
{
    public Int64 Evaluate(Int64 input) => Value + input;
}