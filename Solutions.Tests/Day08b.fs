module Solutions.Tests.Day08b

open System
open Xunit
open Solutions.Day08b

[<Fact>]
let sample () =
    Assert.Equal(
        6L,
        doIt
            """
LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
"""
    )

[<Fact>]
let actual () =
    let file = IO.File.ReadAllText("test-data/day08b")
    printfn "solution = %d" (doIt file)
