using aoc;

static string GetPath(string filename) =>
    // Hacky workaround for stupid dotnet cwd behavior, see:
    // https://github.com/dotnet/project-system/issues/3619
    File.Exists(filename)
        ? Path.Combine(Environment.CurrentDirectory, filename)
        : Path.Combine(Environment.CurrentDirectory, "../../../", filename);

var input = AoC.ParseInput(File.ReadAllLines(GetPath(args[0])));

Console.WriteLine(AoC.Part1(input));
Console.WriteLine(AoC.Part2(input));
