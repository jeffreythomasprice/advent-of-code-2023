module Solutions.Tests.Day06a

open System
open Xunit
open Solutions.Day06a

[<Fact>]
let sample () =
    Assert.Equal(
        288,
        doIt
            """
Time:      7  15   30
Distance:  9  40  200
"""
    )

[<Fact>]
let actual () =
    let file = IO.File.ReadAllText("test-data/day06a")
    printfn "solution = %d" (doIt file)
