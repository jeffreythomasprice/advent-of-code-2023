module Solutions.Tests.Day13a

open System
open Xunit
open Solutions.Day13a

[<Fact>]
let sample () =
    Assert.Equal(
        405,
        doIt
            """#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#"""
    )

[<Fact>]
let actual () =
    let file = IO.File.ReadAllText("test-data/day13a")
    printfn "solution = %d" (doIt file)
