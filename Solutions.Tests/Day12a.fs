module Solutions.Tests.Day12a

open System
open Xunit
open Solutions.Day12a

[<Fact>]
let sample () =
    Assert.Equal(
        21,
        doIt
            """
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
"""
    )

[<Fact>]
let actual () =
    let file = IO.File.ReadAllText("test-data/day12a")
    printfn "solution = %d" (doIt file)
