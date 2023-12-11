module Solutions.Tests.Day11a

open System
open Xunit
open Solutions.Day11a

[<Fact>]
let sample () =
    Assert.Equal(
        374,
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
    let file = IO.File.ReadAllText("test-data/day11a")
    printfn "solution = %d" (doIt file)
