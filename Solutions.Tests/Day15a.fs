module Solutions.Tests.Day15a

open System
open Xunit
open Solutions.Day15a

[<Fact>]
let sample () =
    Assert.Equal(
        1320,
        doIt
            """
rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7
            """
    )

[<Fact>]
let actual () =
    let file = IO.File.ReadAllText("test-data/day15a")
    printfn "solution = %d" (doIt file)
