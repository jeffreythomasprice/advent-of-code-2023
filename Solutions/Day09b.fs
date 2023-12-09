module Solutions.Day09b

open System
open System.Text.RegularExpressions
open System.Collections.Generic

let doIt (input: string) : int =
    let lines =
        input.Split "\n"
        |> Array.toSeq
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter (fun x -> not (String.IsNullOrWhiteSpace x))

    lines
    // conver to ints
    |> Seq.map (fun line ->
        line.Split(" ")
        |> Seq.filter (fun x -> not (String.IsNullOrWhiteSpace x))
        |> Seq.map int
        |> Seq.toList)
    // predict next
    |> Seq.map (fun input ->
        let rec predictPrevious (input: int list) =
            let deltas = List.pairwise input |> List.map (fun (a, b) -> b - a)

            let lastDelta =
                if deltas |> List.forall (fun x -> x = 0) then
                    0
                else
                    predictPrevious deltas

            input[0] - lastDelta

        predictPrevious input)
    |> Seq.sum
