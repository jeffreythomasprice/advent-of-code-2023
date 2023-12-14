module Solutions.Tests.Day13b

open System
open Xunit
open Solutions.Day13b

[<Fact>]
let sample () =
    Assert.Equal(
        400,
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
    let file = IO.File.ReadAllText("test-data/day13b")
    printfn "solution = %d" (doIt file)
