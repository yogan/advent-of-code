open System
open System.IO
open Xunit

type Coord = (int * int)
type Edge = (Coord * Coord)
type Rectangle = (int * int * int * int)

module AoC =
    let Edges (coords: Coord seq) : Edge list =
        let cs = coords |> Seq.toList
        let csShifted = List.tail cs @ [ List.head cs ]
        List.zip cs csShifted |> List.map (fun (l, r) -> if l <= r then l, r else r, l)

    let Inside (p1: Coord) (p2: Coord) (rectangle: Rectangle) : bool =
        let c1, r1 = p1
        let c2, r2 = p2
        let cMin, cMax, rMin, rMax = rectangle

        if r1 = r2 then
            rMin < r1 && r1 < rMax && c1 < cMax && cMin < c2
        else
            cMin < c1 && c1 < cMax && r1 < rMax && rMin < r2

    let Solve (coords: Coord list) : (int64 * int64) =
        let mutable p1 = 0L
        let mutable p2 = 0L
        let E = Edges coords

        coords
        |> List.iteri (fun i (c1, r1) ->
            coords
            |> List.skip i
            |> List.iter (fun (c2, r2) ->
                let area = int64 (abs (c2 - c1) + 1) * int64 (abs (r2 - r1) + 1)
                p1 <- max p1 area

                if area >= p2 then
                    let rectangle = min c1 c2, max c1 c2, min r1 r2, max r1 r2
                    let hasEdgeInside = E |> List.exists (fun (e1, e2) -> Inside e1 e2 rectangle)

                    if not hasEdgeInside then
                        p2 <- area))

        p1, p2

    let ParseInput (lines: string seq) : Coord list =
        lines
        |> Seq.map (fun line ->
            let parts = line.Split ',' |> Array.map int
            parts.[0], parts.[1])
        |> Seq.toList


type AoCTests() =
    [<Fact>]
    member _.``ParseInput works for sample``() =
        let sample = [| "7,1"; "11,1"; "11,7"; "9,7"; "9,5"; "2,5"; "2,3"; "7,3" |]
        let expected = [ 7, 1; 11, 1; 11, 7; 9, 7; 9, 5; 2, 5; 2, 3; 7, 3 ]
        Assert.Equal<Coord list>(expected, AoC.ParseInput sample)

    [<Fact>]
    member _.``Edges works for sample``() =
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

        Assert.Equal<Edge list>(expected, AoC.Edges coords)

    [<Fact>]
    member _.``Solve works for the sample``() =
        let coords = [ 7, 1; 11, 1; 11, 7; 9, 7; 9, 5; 2, 5; 2, 3; 7, 3 ]
        Assert.Equal((50L, 24L), AoC.Solve coords)


let p1, p2 =
    Environment.GetCommandLineArgs()[1]
    |> File.ReadAllLines
    |> AoC.ParseInput
    |> AoC.Solve

printfn "%d" p1
printfn "%d" p2
