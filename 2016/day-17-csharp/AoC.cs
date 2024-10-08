﻿using System.Security.Cryptography;

public enum DoorState { Open, Closed }

public record struct DoorStates(
    DoorState Up, DoorState Down, DoorState Left, DoorState Right);

public record struct Pos(int X, int Y);
public record struct State(Pos Pos, string Path);

public class AoC
{
    private static readonly Pos Start = new Pos(0, 0);
    private static readonly Pos End   = new Pos(3, 3);

    private readonly string code;

    public AoC(string code) { this.code = code; }

    public (string, int) Part1And2() {
        var queue = new Queue<State>(new [] { new State(Start, "") });
        var paths = new List<string>();

        while(queue.Count > 0) {
            var cur = queue.Dequeue();
            if (cur.Pos == End) { paths.Add(cur.Path); continue; }
            Next(cur).ToList().ForEach(queue.Enqueue);
        }

        return (paths.First(), paths.Max(p => p.Length));
    }

    private IEnumerable<State> Next(State state) {
        var (pos, path)             = state;
        var (x, y)                  = pos;
        var (up, down, left, right) = ToDoors(Hash($"{code}{path}"));

        if (up == DoorState.Open && y > 0) {
            yield return new State(new Pos(x, y - 1), $"{path}U");
        }

        if (down == DoorState.Open && y < 3) {
            yield return new State(new Pos(x, y + 1), $"{path}D");
        }

        if (left == DoorState.Open && x > 0) {
            yield return new State(new Pos(x - 1, y), $"{path}L");
        }

        if (right == DoorState.Open && x < 3) {
            yield return new State(new Pos(x + 1, y), $"{path}R");
        }
    }

    public static string Hash(string str) =>
        string.Join("", MD5.Create()
                .ComputeHash(System.Text.Encoding.ASCII.GetBytes(str))
                .Select(b => b.ToString("x2")));

    public static DoorStates ToDoors(string hash) =>
        new DoorStates(
            ToDoorState(hash[0]),
            ToDoorState(hash[1]),
            ToDoorState(hash[2]),
            ToDoorState(hash[3]));

    private static DoorState ToDoorState(char c) =>
        c >= 'b' ? DoorState.Open : DoorState.Closed;
}
