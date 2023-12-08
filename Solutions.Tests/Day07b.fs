module Solutions.Tests.Day07b

open System
open Xunit
open Solutions.Day07b

[<Fact>]
let sample () =
    Assert.Equal(
        5905,
        doIt
            """
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
"""
    )

[<Fact>]
let actual () =
    let file = IO.File.ReadAllText("test-data/day07b")
    printfn "solution = %d" (doIt file)
