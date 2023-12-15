module Solutions.Tests.Day14b

open System
open Xunit
open Solutions.Day14b

[<Fact>]
let sample () =
    Assert.Equal(
        64,
        doIt
            """
O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
            """
    )

[<Fact>]
let actual () =
    let file = IO.File.ReadAllText("test-data/day14b")
    printfn "solution = %d" (doIt file)
