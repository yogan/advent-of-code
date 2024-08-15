static string GetPath(string filename) =>
    // Hacky workaround for stupid dotnet cwd behavior, see:
    // https://github.com/dotnet/project-system/issues/3619
    File.Exists(filename)
        ? Path.Combine(Environment.CurrentDirectory, filename)
        : Path.Combine(Environment.CurrentDirectory, "../../../", filename);

var aoc = new AoC(File.ReadAllText(GetPath(args[0])).Trim());
var (part1, part2) = aoc.Part1And2();
Console.WriteLine(part1);
Console.WriteLine(part2);
