module Solutions.Tests.Day17b

open System
open Xunit
open Solutions.Day17b

[<Fact>]
let sample1 () =
    Assert.Equal(
        94,
        doIt
            """
2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
            """
    )

[<Fact>]
let sample2 () =
    Assert.Equal(
        71,
        doIt
            """
111111111111
999999999991
999999999991
999999999991
999999999991
            """
    )

[<Fact>]
let actual () =
    let file = IO.File.ReadAllText("test-data/day17b")
    printfn "solution = %d" (doIt file)
