module Solutions.Tests.Day01a

open System
open Xunit
open Solutions.Day01a

[<Fact>]
let sample () =
    Assert.Equal(
        142,
        doIt
            """1abc2
            pqr3stu8vwx
            a1b2c3d4e5f
            treb7uchet"""
    )

[<Fact>]
let actual () =
    let file = IO.File.ReadAllText("test-data/day01a")
    printfn "solution = %d" (doIt file)
