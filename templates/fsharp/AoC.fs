open System
open System.IO
open Xunit

type Box =
    { Length: int
      Width: int
      Height: int }

    member this.Volume = this.Length * this.Width * this.Height

module AoC =
    let Part1 (boxes: Box seq) =
        boxes |> Seq.sumBy (fun box -> box.Volume)

    let ParseInput (lines: string seq) =
        lines
        |> Seq.map (fun line ->
            let parts = line.Split 'x' |> Array.map int

            { Length = parts[0]
              Width = parts[1]
              Height = parts[2] })


type AoCTests() =
    [<Fact>]
    member _.``ParseInput works for sample``() =
        Assert.Equal<Box[]>(
            [| { Length = 1; Width = 2; Height = 3 }
               { Length = 987; Width = 10; Height = 1 } |],
            AoC.ParseInput [| "1x2x3"; "987x10x1" |] |> Seq.toArray
        )

    [<Fact>]
    member _.``Part1 returns the sum of the volumes``() =
        Assert.Equal(6 + 9870, [| "1x2x3"; "987x10x1" |] |> AoC.ParseInput |> AoC.Part1)


let input =
    Environment.GetCommandLineArgs()[1] |> File.ReadAllLines |> AoC.ParseInput

input |> AoC.Part1 |> printfn "%d"
