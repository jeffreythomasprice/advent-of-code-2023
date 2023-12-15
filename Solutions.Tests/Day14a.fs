module Solutions.Tests.Day14a

open System
open Xunit
open Solutions.Day14a

[<Fact>]
let sample () =
    Assert.Equal(
        136,
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
    let file = IO.File.ReadAllText("test-data/day14a")
    printfn "solution = %d" (doIt file)
