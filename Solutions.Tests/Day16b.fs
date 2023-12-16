module Solutions.Tests.Day16b

open System
open Xunit
open Solutions.Day16b

[<Fact>]
let sample () =
    Assert.Equal(
        51,
        doIt
            """
.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....
            """
    )

[<Fact>]
let actual () =
    let file = IO.File.ReadAllText("test-data/day16b")
    printfn "solution = %d" (doIt file)
