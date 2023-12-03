module Solutions.Tests.Day03a

open System
open Xunit
open Solutions.Day03a

[<Fact>]
let sample () =
    Assert.Equal(
        4361,
        doIt
            """
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
"""
    )

[<Fact>]
let actual () =
    let file = IO.File.ReadAllText("test-data/day03a")
    printfn "solution = %d" (doIt file)
