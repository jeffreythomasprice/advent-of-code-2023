module Solutions.Tests.Day01b

open System
open Xunit
open Solutions.Day01b

[<Fact>]
let sample () =
    Assert.Equal(
        281,
        doIt
            """two1nine
            eightwothree
            abcone2threexyz
            xtwone3four
            4nineeightseven2
            zoneight234
            7pqrstsixteen"""
    )

[<Fact>]
let actual () =
    let file = IO.File.ReadAllText("test-data/day01b")
    printfn "solution = %d" (doIt file)
