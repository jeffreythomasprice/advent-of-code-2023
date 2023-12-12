module Solutions.Tests.Day11b

open System
open Xunit
open Solutions.Day11b

[<Fact>]
let sample () =
    Assert.Equal(
        82000210L,
        doIt
            """
...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
"""
    )

[<Fact>]
let actual () =
    let file = IO.File.ReadAllText("test-data/day11b")
    printfn "solution = %d" (doIt file)
