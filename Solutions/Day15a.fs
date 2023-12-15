module Solutions.Day15a

open System
open System.Text.RegularExpressions
open System.Collections.Generic

let hash (s: string) : int =
    s
    |> Seq.map (fun c -> c |> int)
    |> Seq.fold (fun result x -> ((result + x) * 17) % 256) 0

let doIt (input: string) : int =
    let lines =
        input.Split "\n"
        |> Array.toSeq
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter (fun x -> not (String.IsNullOrWhiteSpace x))

    if lines |> Seq.length <> 1 then
        failwith "expected a single line of input"

    let input = lines |> Seq.head
    let input = input.Split(",")

    input |> Seq.map hash |> Seq.sum
