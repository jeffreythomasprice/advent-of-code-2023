module Solutions.Tests.Day09a

open System
open Xunit
open Solutions.Day09a

[<Fact>]
let sample () =
    Assert.Equal(
        114,
        doIt
            """
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
"""
    )

[<Fact>]
let actual () =
    let file = IO.File.ReadAllText("test-data/day09a")
    printfn "solution = %d" (doIt file)
