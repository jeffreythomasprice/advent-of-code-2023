module Solutions.Tests.Day16a

open System
open Xunit
open Solutions.Day16a

[<Fact>]
let sample () =
    Assert.Equal(
        46,
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
    let file = IO.File.ReadAllText("test-data/day16a")
    printfn "solution = %d" (doIt file)
