public record struct Monkey(
    Stack<int> Items,
    Operation Operation,
    int Divisor,
    int TargetTrue,
    int TargetFalse
);

public interface Operation
{
    int Evaluate(int input);
}

public record struct Square : Operation
{
    public int Evaluate(int input) => input * input;
}

public record struct Multiply(int Value) : Operation
{
    public int Evaluate(int input) => Value * input;
}

public record struct Add(int Value) : Operation
{
    public int Evaluate(int input) => Value + input;
}

public class Day11
{
    public static int Part1(string filename = "day11.in")
    {
        var monkeys = ParseInput(filename);
        var inspections = SimulateMonkeyBusiness(monkeys, 20);
        Array.Sort(inspections);
        Array.Reverse(inspections);
        return inspections[0] * inspections[1];
    }

    public static IList<Monkey> ParseInput(string filename) =>
        File
            .ReadAllText(GetPath(filename))
            .Split("\n\n")
            .Select(group => group.Split('\n'))
            .Select(lines => new Monkey(
                Items: ParseItems(lines[1]),
                Operation: ParseOperation(lines[2]),
                Divisor: ParseDivisor(lines[3]),
                TargetTrue: ParseTarget(lines[4]),
                TargetFalse: ParseTarget(lines[5])
            ))
            .ToList();

    public static int[] SimulateMonkeyBusiness(IEnumerable<Monkey> monkeys, int rounds)
    {
        var inspections = new int[monkeys.Count()];

        for (int round = 0; round < rounds; round++)
        {
            for (int m = 0; m < monkeys.Count(); m++)
            {
                var monkey = monkeys.ElementAt(m);
                foreach (var item in monkey.Items)
                {
                    inspections[m]++;
                    int worryLevel = monkey.Operation.Evaluate(item) / 3;
                    var target = worryLevel % monkey.Divisor == 0
                        ? monkey.TargetTrue
                        : monkey.TargetFalse;
                    monkeys.ElementAt(target).Items.Push(worryLevel);
                }
                monkey.Items.Clear();
            }
        }

        return inspections;
    }

    private static Stack<int> ParseItems(string line) =>
        new Stack<int>(
            line.Split(':').Last().Split(',').Select(int.Parse)
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