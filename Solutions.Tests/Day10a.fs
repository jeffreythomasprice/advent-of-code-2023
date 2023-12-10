module Solutions.Tests.Day10a

open System
open Xunit
open Solutions.Day10a

[<Fact>]
let sample1 () =
    Assert.Equal(
        4,
        doIt
            """
-L|F7
7S-7|
L|7||
-L-J|
L|-JF
"""
    )

[<Fact>]
let sample2 () =
    Assert.Equal(
        8,
        doIt
            """
7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ
"""
    )

[<Fact>]
let actual () =
    let file = IO.File.ReadAllText("test-data/day10a")
    printfn "solution = %d" (doIt file)
