module Solutions.Tests.Day20a

open System
open Xunit
open Solutions.Day20a

[<Fact>]
let sample1 () =
    Assert.Equal(
        32000000,
        doIt
            """
broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a
            """
    )

[<Fact>]
let sample2 () =
    Assert.Equal(
        11687500,
        doIt
            """
broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output
            """
    )

[<Fact>]
let actual () =
    let file = IO.File.ReadAllText("test-data/day20a")
    printfn "solution = %d" (doIt file)
