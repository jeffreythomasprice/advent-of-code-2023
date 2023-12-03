module Solutions.Tests.Day03b

open System
open Xunit
open Solutions.Day03b

[<Fact>]
let sample () =
    Assert.Equal(
        467835,
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
    let file = IO.File.ReadAllText("test-data/day03b")
    printfn "solution = %d" (doIt file)
