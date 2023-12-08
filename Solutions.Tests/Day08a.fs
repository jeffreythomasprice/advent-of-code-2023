module Solutions.Tests.Day08a

open System
open Xunit
open Solutions.Day08a

[<Fact>]
let sample1 () =
    Assert.Equal(
        2,
        doIt
            """
RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
"""
    )

[<Fact>]
let sample2 () =
    Assert.Equal(
        6,
        doIt
            """
LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
"""
    )

[<Fact>]
let actual () =
    let file = IO.File.ReadAllText("test-data/day08a")
    printfn "solution = %d" (doIt file)
