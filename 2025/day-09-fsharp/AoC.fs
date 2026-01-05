open System
open System.IO
open Xunit

type Coord = (int * int)
type Edge = (Coord * Coord)
type Rectangle = (int * int * int * int)

module AoC =
    let edges (coords: Coord seq) : Edge list =
        let cs = coords |> Seq.toList
        let csShifted = List.tail cs @ [ List.head cs ]
        List.zip cs csShifted |> List.map (fun (l, r) -> if l <= r then l, r else r, l)

    let solve (coords: Coord list) : (int64 * int64) =
        let area (c1, r1) (c2, r2) =
            int64 (abs (c2 - c1) + 1) * int64 (abs (r2 - r1) + 1)

        let combinations list =
            list
            |> List.indexed
            |> List.collect (fun (i, x) -> list |> List.skip i |> List.map (fun y -> x, y))

        let edgeWithin edges (c1, r1) (c2, r2) =
            let cMin, cMax, rMin, rMax = min c1 c2, max c1 c2, min r1 r2, max r1 r2

            let inside (c1, r1) (c2, r2) =
                if r1 = r2 then
                    rMin < r1 && r1 < rMax && c1 < cMax && cMin < c2
                else
                    cMin < c1 && c1 < cMax && r1 < rMax && rMin < r2

            edges |> List.exists (fun (e1, e2) -> inside e1 e2)

        let edges = edges coords

        let tryPair (p1, p2) (c1, c2) =
            let a = area c1 c2
            let p1 = max p1 a
            let p2 = if a >= p2 && not (edgeWithin edges c1 c2) then a else p2
            p1, p2

        coords |> combinations |> List.fold tryPair (0L, 0L)

    let parse (lines: string seq) : Coord list =
        lines
        |> Seq.map (fun line ->
            let parts = line.Split ',' |> Array.map int
            parts.[0], parts.[1])
        |> Seq.toList


type AoCTests() =
    [<Fact>]
    member _.``parse works for sample``() =
        let sample = [| "7,1"; "11,1"; "11,7"; "9,7"; "9,5"; "2,5"; "2,3"; "7,3" |]
        let expected = [ 7, 1; 11, 1; 11, 7; 9, 7; 9, 5; 2, 5; 2, 3; 7, 3 ]
        Assert.Equal<Coord list>(expected, AoC.parse sample)

    [<Fact>]
    member _.``edges works for sample``() =
        let coords = [ 7, 1; 11, 1; 11, 7; 9, 7; 9, 5; 2, 5; 2, 3; 7, 3 ]

        let expected: Edge list =
            [ (7, 1), (11, 1)
              (11, 1), (11, 7)
              (9, 7), (11, 7)
              (9, 5), (9, 7)
              (2, 5), (9, 5)
              (2, 3), (2, 5)
              (2, 3), (7, 3)
              (7, 1), (7, 3) ]

        Assert.Equal<Edge list>(expected, AoC.edges coords)

    [<Fact>]
    member _.``solve works for the sample``() =
        let coords = [ 7, 1; 11, 1; 11, 7; 9, 7; 9, 5; 2, 5; 2, 3; 7, 3 ]
        Assert.Equal((50L, 24L), AoC.solve coords)


let p1, p2 =
    Environment.GetCommandLineArgs()[1]
    |> File.ReadAllLines
    |> AoC.parse
    |> AoC.solve

printfn "%d" p1
printfn "%d" p2
