module Solutions.Tests.Day06b

open System
open Xunit
open Solutions.Day06b

[<Fact>]
let sample () =
    Assert.Equal(
        71503L,
        doIt
            """
Time:      7  15   30
Distance:  9  40  200
"""
    )

[<Fact>]
let actual () =
    let file = IO.File.ReadAllText("test-data/day06b")
    printfn "solution = %d" (doIt file)
