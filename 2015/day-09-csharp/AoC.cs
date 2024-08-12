public record struct Graph(IEnumerable<Edge> Edges, IEnumerable<string> Nodes);
public record struct Edge(string A, string B, int Distance);

public class AoC
{
    public static int Part1(Graph G) =>
        Permutations(G.Nodes, G.Nodes.Count())
            .Select(p => p.Zip(p.Skip(1), (a, b) => GetEdge(G, a, b).Distance).Sum())
            .Min();

    public static Graph ParseInput(IEnumerable<string> lines) {
        var edges = lines.SelectMany(line => {
            var parts = line.Split(" ");
            var (a, b, dist) = (parts[0], parts[2], int.Parse(parts[4]));
            return new [] { new Edge(a, b, dist), new Edge(b, a, dist) };
        });

        var nodes = edges.SelectMany(e => new [] { e.A, e.B }).Distinct();

        return new Graph(edges, nodes);
    }

    private static IEnumerable<IEnumerable<T>> Permutations<T>(
            IEnumerable<T> list, int length) {
        if (length == 1) { return list.Select(t => new T[] { t }); }

        return Permutations(list, length - 1)
            .SelectMany(t => list.Where(e => !t.Contains(e)),
                    (t1, t2) => t1.Concat(new T[] { t2 }));
    }

    private static Edge GetEdge(Graph G, string a, string b) =>
        G.Edges.First(e => e.A == a && e.B == b);
}
